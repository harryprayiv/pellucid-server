{-# LANGUAGE QuasiQuotes #-}

{- | Posts, comments, and the lists they are read in. Every list of posts
is newest first by id and takes a cursor; none of them inserts or
reorders anything (S10). A thread is read oldest first.
-}
module Pellucid.Store.Posts (
  PostRow (..),
  postColumns,
  readableBy,
  NewPost (..),
  insertPost,
  NewComment (..),
  insertComment,
  findPost,
  postsReadableBy,
  withdrawPost,
  homeTimeline,
  publicTimeline,
  accountTimeline,
  ancestorsOf,
  descendantsOf,
  countLocalPosts,
) where

import Control.Monad (forM, forM_, void)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), Query, execute, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pellucid.Domain.Cursor (Cursor (..))
import Pellucid.Domain.Ids (AccountId (..), MediaId (..), PostId (..))
import Pellucid.Domain.Post (PostStatus, Visibility, parsePostStatus, parseVisibility, visibilityText)
import Pellucid.Store (parsed, transact)
import Pellucid.Store.Media (MediaRow, mediaOfPosts)
import Pellucid.Store.Notifications (forgetPost, notifyComment)

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
  , threadRoot :: Maybe PostId
  -- ^ For a comment, the post at the top of its thread.
  , parentAuthor :: Maybe AccountId
  -- ^ For a comment, the author of what it answers.
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
      <*> (fmap PostId <$> field)
      <*> (fmap AccountId <$> field)

-- | The columns of a 'PostRow', for a query that reads @posts p@.
postColumns :: Query
postColumns =
  [sql|
    p.id, p.author_id, p.caption, p.visibility, p.sensitive, p.spoiler_text, p.comments_disabled,
    p.in_reply_to, p.published_at, p.edit_count, p.status,
    (SELECT count(*) FROM posts r WHERE r.in_reply_to = p.id AND r.status = 'published'),
    p.thread_root,
    (SELECT q.author_id FROM posts q WHERE q.id = p.in_reply_to)
  |]

selectPost :: Query
selectPost = "SELECT " <> postColumns <> " FROM posts p"

{- | The spec's @can_see@ for @posts p@ and a signed-in reader, as a
condition with three placeholders, each of them the reader. A post is
read under its own visibility; a comment is read by its author and by
whoever can read the post at the top of its thread.
"Pellucid.Domain.Post" states the same rule for one post at a time, and
the end-to-end test asks both the same questions.
-}
readableBy :: Query
readableBy =
  [sql|
    p.status = 'published'
    AND (p.author_id = ?
         OR EXISTS (SELECT 1 FROM posts top
                    WHERE top.id = COALESCE(p.thread_root, p.id) AND top.status = 'published'
                      AND (top.visibility IN ('public', 'unlisted')
                           OR top.author_id = ?
                           OR (top.visibility = 'followers_only'
                               AND EXISTS (SELECT 1 FROM follows f
                                           WHERE f.follower_id = ? AND f.target_id = top.author_id
                                             AND f.status = 'following')))))
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

data NewComment = NewComment
  { author :: AccountId
  , text :: Text
  , parent :: PostId
  , publishedAt :: UTCTime
  }

{- | Rule CommentPublished's effect in one transaction: the comment, with
its parent's visibility and its thread's top, and the notice to the
account named, if one is. The comment is written only if the parent is
still published and still takes comments at this moment; if it is not,
nothing is written and the answer is 'Nothing'.
-}
insertComment :: Connection -> NewComment -> Maybe AccountId -> IO (Maybe PostId)
insertComment conn new tell = do
  let
    AccountId author = new.author
    PostId parent = new.parent
  result <- transact conn do
    rows <-
      query
        conn
        [sql|
          INSERT INTO posts (author_id, caption, visibility, in_reply_to, thread_root, published_at)
          SELECT ?, ?, parent.visibility, parent.id, COALESCE(parent.thread_root, parent.id), ?
          FROM posts parent
          WHERE parent.id = ? AND parent.status = 'published' AND NOT parent.comments_disabled
          RETURNING id
        |]
        (author, new.text, new.publishedAt, parent)
    case rows of
      [] -> pure (Left ())
      Only comment : _ -> do
        forM_ tell \recipient -> notifyComment conn recipient new.author new.parent (PostId comment) new.publishedAt
        pure (Right (PostId comment))
  pure (either (const Nothing) Just result)

findPost :: Connection -> PostId -> IO (Maybe PostRow)
findPost conn (PostId n) = listToMaybe <$> query conn (selectPost <> " WHERE p.id = ?") (Only n)

{- | Those of the given posts that the reader may see. Used where a post
arrives beside something else, as in a notification (guarantee
ReadsObeyTheReadingRules).
-}
postsReadableBy :: Connection -> AccountId -> [PostId] -> IO [PostRow]
postsReadableBy _ _ [] = pure []
postsReadableBy conn (AccountId reader) posts =
  query
    conn
    (selectPost <> " WHERE p.id IN ? AND " <> readableBy)
    (In [n | PostId n <- posts], reader, reader, reader)

{- | Rule PostWithdrawn's effect with rule MediaReleased for each
attachment: the post becomes withdrawn, its media purged, and its likes,
its bookmarks and every notification about it removed. Returns the
media rows as they were, so the caller can remove their files, or
'Nothing' when the post is not the author's or cannot be withdrawn.
Comments under it are their own authors' posts and stay.
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
        void (execute conn "DELETE FROM likes WHERE post_id = ?" (Only post))
        void (execute conn "DELETE FROM bookmarks WHERE post_id = ?" (Only post))
        forgetPost conn (PostId post)
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
the caller does for the author and for an accepted follower. Comments
are not in it.
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

{- | What a comment answers, from the top of its thread down to its parent.
A post has none. Only published posts are in the list, so a thread whose
top has been deleted shows what is left of it.
-}
ancestorsOf :: Connection -> PostId -> IO [PostRow]
ancestorsOf conn (PostId post) =
  query
    conn
    ( [sql|
        WITH RECURSIVE chain (id, parent, depth) AS (
          SELECT c.id, c.in_reply_to, 0 FROM posts c WHERE c.id = ?
          UNION ALL
          SELECT c.id, c.in_reply_to, chain.depth + 1 FROM posts c JOIN chain ON c.id = chain.parent
        )
      |]
        <> selectPost
        <> " JOIN chain ON chain.id = p.id WHERE chain.depth > 0 AND p.status = 'published' ORDER BY chain.depth DESC"
    )
    (Only post)

{- | The published comments under a post or a comment, at any depth, oldest
first, up to a number of them. A comment is reached through published
comments only.
-}
descendantsOf :: Connection -> PostId -> Int -> IO [PostRow]
descendantsOf conn (PostId post) rows =
  query
    conn
    ( [sql|
        WITH RECURSIVE thread (id) AS (
          SELECT c.id FROM posts c WHERE c.in_reply_to = ? AND c.status = 'published'
          UNION ALL
          SELECT c.id FROM posts c JOIN thread ON c.in_reply_to = thread.id WHERE c.status = 'published'
        )
      |]
        <> selectPost
        <> " WHERE p.id IN (SELECT id FROM thread) ORDER BY p.id LIMIT ?"
    )
    (post, rows)

countLocalPosts :: Connection -> IO Int
countLocalPosts conn =
  maybe 0 fromOnly . listToMaybe
    <$> query_
      conn
      "SELECT count(*) FROM posts p JOIN accounts a ON a.id = p.author_id WHERE a.home IS NULL AND p.status = 'published' AND p.in_reply_to IS NULL"

-- | The id a list starts below: the cursor's, or past the largest id.
below :: Maybe Cursor -> Int64
below = maybe maxBound (\(Cursor n) -> n)
