{-# LANGUAGE QuasiQuotes #-}

{- | Posts and the lists they are read in. Every list is newest first by
id and takes a cursor; none of them inserts or reorders anything (S10).
-}
module Pellucid.Store.Posts (
  PostRow (..),
  NewPost (..),
  insertPost,
  findPost,
  withdrawPost,
  homeTimeline,
  publicTimeline,
  accountTimeline,
  countLocalPosts,
) where

import Control.Monad (forM, void)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), Query, execute, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pellucid.Domain.Cursor (Cursor (..))
import Pellucid.Domain.Ids (AccountId (..), MediaId (..), PostId (..))
import Pellucid.Domain.Post (PostStatus, Visibility, parsePostStatus, parseVisibility, visibilityText)
import Pellucid.Store (parsed, transact)
import Pellucid.Store.Media (MediaRow, mediaOfPosts)

data PostRow = PostRow
  { postId :: PostId
  , author :: AccountId
  , caption :: Text
  , visibility :: Visibility
  , sensitive :: Bool
  , spoilerText :: Text
  , commentsDisabled :: Bool
  , inReplyTo :: Maybe PostId
  , publishedAt :: UTCTime
  , editCount :: Int
  , status :: PostStatus
  , replyCount :: Int
  }

instance FromRow PostRow where
  fromRow =
    PostRow
      <$> (PostId <$> field)
      <*> (AccountId <$> field)
      <*> field
      <*> parsed parseVisibility
      <*> field
      <*> field
      <*> field
      <*> (fmap PostId <$> field)
      <*> field
      <*> field
      <*> parsed parsePostStatus
      <*> field

selectPost :: Query
selectPost =
  [sql|
    SELECT p.id, p.author_id, p.caption, p.visibility, p.sensitive, p.spoiler_text, p.comments_disabled,
           p.in_reply_to, p.published_at, p.edit_count, p.status,
           (SELECT count(*) FROM posts r WHERE r.in_reply_to = p.id AND r.status = 'published')
    FROM posts p
  |]

data NewPost = NewPost
  { author :: AccountId
  , caption :: Text
  , visibility :: Visibility
  , sensitive :: Bool
  , spoilerText :: Text
  , commentsDisabled :: Bool
  , publishedAt :: UTCTime
  }

{- | Rule PostPublished's effect, with rule MediaAttached for each
attachment, in one transaction. An attachment is taken only if it is
still the author's and still ready at this moment (invariant
AttachedMediaBelongsToItsPostsAuthor); if any one is not, nothing is
written and the answer is 'Nothing'. That closes the gap between the
check the caller made and this write.
-}
insertPost :: Connection -> NewPost -> [MediaId] -> IO (Maybe PostId)
insertPost conn new attachments = do
  let AccountId author = new.author
  result <- transact conn do
    rows <-
      query
        conn
        [sql|
          INSERT INTO posts (author_id, caption, visibility, sensitive, spoiler_text, comments_disabled, published_at)
          VALUES (?, ?, ?, ?, ?, ?, ?)
          RETURNING id
        |]
        (author, new.caption, visibilityText new.visibility, new.sensitive, new.spoilerText, new.commentsDisabled, new.publishedAt)
    case rows of
      [] -> pure (Left ())
      Only post : _ -> do
        taken <- forM (zip [0 :: Int ..] attachments) \(position, MediaId media) ->
          execute
            conn
            "UPDATE media SET status = 'attached', post_id = ?, position = ? WHERE id = ? AND owner_id = ? AND status = 'ready'"
            (post :: Int64, position, media, author)
        pure (if all (== 1) taken then Right (PostId post) else Left ())
  pure (either (const Nothing) Just result)

findPost :: Connection -> PostId -> IO (Maybe PostRow)
findPost conn (PostId n) = listToMaybe <$> query conn (selectPost <> " WHERE p.id = ?") (Only n)

{- | Rule PostWithdrawn's effect with rule MediaReleased for each
attachment: the post becomes withdrawn and its media purged. Returns the
media rows as they were, so the caller can remove their files, or
'Nothing' when the post is not the author's or cannot be withdrawn.
-}
withdrawPost :: Connection -> PostId -> AccountId -> IO (Maybe [MediaRow])
withdrawPost conn (PostId post) (AccountId author) = do
  result <- transact conn do
    changed <- execute conn "UPDATE posts SET status = 'withdrawn' WHERE id = ? AND author_id = ? AND status IN ('published', 'taken_down')" (post, author)
    if changed /= 1
      then pure (Left ())
      else do
        media <- mediaOfPosts conn [PostId post]
        void (execute conn "UPDATE media SET status = 'purged', post_id = NULL, position = NULL WHERE post_id = ?" (Only post))
        pure (Right media)
  pure (either (const Nothing) Just result)

{- | The spec's home timeline: the published posts of the viewer and of the
accounts the viewer follows, that the viewer can see. Comments are not
in it. A post addressed to nobody but its recipients is not in it.
-}
homeTimeline :: Connection -> AccountId -> Maybe Cursor -> Int -> IO [PostRow]
homeTimeline conn (AccountId viewer) cursor rows =
  query
    conn
    ( selectPost
        <> " "
        <> [sql|
             WHERE p.status = 'published' AND p.in_reply_to IS NULL
               AND p.visibility IN ('public', 'unlisted', 'followers_only')
               AND (p.author_id = ?
                    OR EXISTS (SELECT 1 FROM follows f
                               WHERE f.follower_id = ? AND f.target_id = p.author_id AND f.status = 'following'))
               AND p.id < ?
             ORDER BY p.id DESC LIMIT ?
           |]
    )
    (viewer, viewer, below cursor, rows)

{- | The spec's public timeline: public posts by local accounts. The
reach limit that would keep an account out of it is not built.
-}
publicTimeline :: Connection -> Maybe Cursor -> Int -> IO [PostRow]
publicTimeline conn cursor rows =
  query
    conn
    ( selectPost
        <> " "
        <> [sql|
             WHERE p.status = 'published' AND p.in_reply_to IS NULL AND p.visibility = 'public'
               AND EXISTS (SELECT 1 FROM accounts a WHERE a.id = p.author_id AND a.home IS NULL)
               AND p.id < ?
             ORDER BY p.id DESC LIMIT ?
           |]
    )
    (below cursor, rows)

{- | One account's posts as a given reader may see them: public and
unlisted for anyone, followers-only as well when 'True' is passed, which
the caller does for the author and for an accepted follower.
-}
accountTimeline :: Connection -> AccountId -> Bool -> Maybe Cursor -> Int -> IO [PostRow]
accountTimeline conn (AccountId author) includeFollowersOnly cursor rows =
  query
    conn
    ( selectPost
        <> " "
        <> [sql|
             WHERE p.author_id = ? AND p.status = 'published' AND p.in_reply_to IS NULL
               AND (p.visibility IN ('public', 'unlisted') OR (? AND p.visibility = 'followers_only'))
               AND p.id < ?
             ORDER BY p.id DESC LIMIT ?
           |]
    )
    (author, includeFollowersOnly, below cursor, rows)

countLocalPosts :: Connection -> IO Int
countLocalPosts conn =
  maybe 0 fromOnly . listToMaybe
    <$> query_ conn "SELECT count(*) FROM posts p JOIN accounts a ON a.id = p.author_id WHERE a.home IS NULL AND p.status = 'published'"

-- | The id a list starts below: the cursor's, or past the largest id.
below :: Maybe Cursor -> Int64
below = maybe maxBound (\(Cursor n) -> n)
