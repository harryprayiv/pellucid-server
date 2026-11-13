{-# LANGUAGE QuasiQuotes #-}

{- | Likes and bookmarks: placing one, taking one back, and the lists they
are read in. A list is newest first by when the like or the bookmark was
placed, which is the order of its own id, and pages by that id.
-}
module Pellucid.Store.Interactions (
  hasLiked,
  hasBookmarked,
  insertLike,
  deleteLike,
  insertBookmark,
  deleteBookmark,
  Marks (..),
  marksOf,
  likersOf,
  likedPosts,
  bookmarkedPosts,
) where

import Control.Monad (forM_, void, when)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), Query, execute, query, (:.) (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pellucid.Domain.Cursor (Cursor (..))
import Pellucid.Domain.Ids (AccountId (..), PostId (..))
import Pellucid.Store (transact)
import Pellucid.Store.Notifications (forgetLike, notifyLike)
import Pellucid.Store.Posts (PostRow, postColumns, readableBy)

hasLiked :: Connection -> AccountId -> PostId -> IO Bool
hasLiked conn (AccountId account) (PostId post) =
  not . null <$> (query conn "SELECT 1 FROM likes WHERE account_id = ? AND post_id = ?" (account, post) :: IO [Only Int])

hasBookmarked :: Connection -> AccountId -> PostId -> IO Bool
hasBookmarked conn (AccountId account) (PostId post) =
  not . null <$> (query conn "SELECT 1 FROM bookmarks WHERE account_id = ? AND post_id = ?" (account, post) :: IO [Only Int])

{- | Rule PostLiked's effect in one transaction: the like and, when an
account is named, the notice to it. The unique key makes a second like
of the same post a no-op, and then nobody is told again.
-}
insertLike :: Connection -> AccountId -> PostId -> Maybe AccountId -> UTCTime -> IO ()
insertLike conn account post tell now = void . transact conn $ do
  placed <- execute conn "INSERT INTO likes (account_id, post_id) VALUES (?, ?) ON CONFLICT (account_id, post_id) DO NOTHING" (accountNumber, postNumber)
  when (placed == 1) (forM_ tell \author -> notifyLike conn author account post now)
  pure (Right () :: Either () ())
  where
    AccountId accountNumber = account
    PostId postNumber = post

-- | Rule PostUnliked. The notice of the like goes with it (decision S20).
deleteLike :: Connection -> AccountId -> PostId -> IO ()
deleteLike conn account post = void . transact conn $ do
  void (execute conn "DELETE FROM likes WHERE account_id = ? AND post_id = ?" (accountNumber, postNumber))
  forgetLike conn account post
  pure (Right () :: Either () ())
  where
    AccountId accountNumber = account
    PostId postNumber = post

-- | Rule PostBookmarked. Nobody is told.
insertBookmark :: Connection -> AccountId -> PostId -> IO ()
insertBookmark conn (AccountId account) (PostId post) =
  void (execute conn "INSERT INTO bookmarks (account_id, post_id) VALUES (?, ?) ON CONFLICT (account_id, post_id) DO NOTHING" (account, post))

-- | Rule PostUnbookmarked.
deleteBookmark :: Connection -> AccountId -> PostId -> IO ()
deleteBookmark conn (AccountId account) (PostId post) =
  void (execute conn "DELETE FROM bookmarks WHERE account_id = ? AND post_id = ?" (account, post))

-- | What a post's entity says about likes and bookmarks, for one reader.
data Marks = Marks
  { post :: PostId
  , likes :: Int
  , likedByReader :: Bool
  , bookmarkedByReader :: Bool
  , latestLiker :: Maybe (AccountId, Text)
  -- ^ The account that liked the post most recently, with its username.
  }

instance FromRow Marks where
  fromRow = do
    post <- PostId <$> field
    likes <- field
    likedByReader <- field
    bookmarkedByReader <- field
    liker <- field
    username <- field
    pure Marks {post, likes, likedByReader, bookmarkedByReader, latestLiker = (,) . AccountId <$> liker <*> username}

{- | The marks of a page of posts in one query. With no reader, nothing is
liked or bookmarked by the reader.
-}
marksOf :: Connection -> Maybe AccountId -> [PostId] -> IO [Marks]
marksOf _ _ [] = pure []
marksOf conn reader posts =
  query
    conn
    [sql|
      SELECT p.id,
        (SELECT count(*) FROM likes l WHERE l.post_id = p.id),
        EXISTS (SELECT 1 FROM likes l WHERE l.post_id = p.id AND l.account_id = ?),
        EXISTS (SELECT 1 FROM bookmarks b WHERE b.post_id = p.id AND b.account_id = ?),
        (SELECT l.account_id FROM likes l WHERE l.post_id = p.id ORDER BY l.id DESC LIMIT 1),
        (SELECT a.username FROM likes l JOIN accounts a ON a.id = l.account_id WHERE l.post_id = p.id ORDER BY l.id DESC LIMIT 1)
      FROM posts p WHERE p.id IN ?
    |]
    (readerNumber, readerNumber, In [n | PostId n <- posts])
  where
    readerNumber :: Maybe Int64
    readerNumber = fmap (\(AccountId n) -> n) reader

-- | The accounts that like a post, newest like first, each with the like's id.
likersOf :: Connection -> PostId -> Maybe Cursor -> Int -> IO [(Int64, AccountId)]
likersOf conn (PostId post) cursor rows = do
  found <- query conn "SELECT id, account_id FROM likes WHERE post_id = ? AND id < ? ORDER BY id DESC LIMIT ?" (post, below cursor, rows)
  pure [(like, AccountId account) | (like, account) <- found]

{- | The posts an account likes that it can still read, newest like first,
each with the like's id.
-}
likedPosts :: Connection -> AccountId -> Maybe Cursor -> Int -> IO [(Int64, PostRow)]
likedPosts = markedPosts "likes"

-- | The same for bookmarks.
bookmarkedPosts :: Connection -> AccountId -> Maybe Cursor -> Int -> IO [(Int64, PostRow)]
bookmarkedPosts = markedPosts "bookmarks"

{- | The table's name is one of two literals in this module and never
comes from a request.
-}
markedPosts :: Query -> Connection -> AccountId -> Maybe Cursor -> Int -> IO [(Int64, PostRow)]
markedPosts table conn (AccountId account) cursor rows = do
  found <-
    query
      conn
      ( "SELECT m.id, "
          <> postColumns
          <> " FROM "
          <> table
          <> " m JOIN posts p ON p.id = m.post_id WHERE m.account_id = ? AND m.id < ? AND "
          <> readableBy
          <> " ORDER BY m.id DESC LIMIT ?"
      )
      (account, below cursor, account, account, account, rows)
  pure [(mark, row) | Only mark :. row <- found]

-- | The id a list starts below: the cursor's, or past the largest id.
below :: Maybe Cursor -> Int64
below = maybe maxBound (\(Cursor n) -> n)
