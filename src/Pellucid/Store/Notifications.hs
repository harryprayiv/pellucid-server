{-# LANGUAGE QuasiQuotes #-}

{- | Notifications. One is written in the transaction of the rule that
causes it, and removed in the transaction that takes away what it
reports (decision S20): a like that is taken back, a follow that ends, a
comment or a post that is deleted.
-}
module Pellucid.Store.Notifications (
  NotificationRow (..),
  notifyFollow,
  notifyLike,
  notifyComment,
  forgetFollow,
  forgetLike,
  forgetPost,
  notificationsFor,
  markSeen,
  unreadCount,
) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), execute, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pellucid.Domain.Cursor (Cursor (..))
import Pellucid.Domain.Ids (AccountId (..), PostId (..))
import Pellucid.Domain.Notification (NotificationKind (..), kindText, parseKind)
import Pellucid.Store (parsed)

data NotificationRow = NotificationRow
  { notificationId :: Int64
  , kind :: NotificationKind
  , actor :: Maybe AccountId
  , post :: Maybe PostId
  -- ^ The post the notification is about: the one liked or commented on.
  , comment :: Maybe PostId
  -- ^ The comment itself, when the notification reports one.
  , createdAt :: UTCTime
  }

instance FromRow NotificationRow where
  fromRow =
    NotificationRow
      <$> field
      <*> parsed parseKind
      <*> (fmap AccountId <$> field)
      <*> (fmap PostId <$> field)
      <*> (fmap PostId <$> field)
      <*> field

{- | Rules FollowedOpenAccount and FollowRequestedOfLockedAccount: the
target is told. A second notice for the same pair is not written.
-}
notifyFollow :: Connection -> NotificationKind -> AccountId -> AccountId -> UTCTime -> IO ()
notifyFollow conn kind (AccountId follower) (AccountId target) now =
  void
    ( execute
        conn
        [sql|
          INSERT INTO notifications (recipient_id, actor_id, kind, created_at) VALUES (?, ?, ?, ?)
          ON CONFLICT (actor_id, recipient_id) WHERE kind IN ('followed', 'follow_requested') DO NOTHING
        |]
        (target, follower, kindText kind, now)
    )

-- | Rule PostLiked: the post's author is told.
notifyLike :: Connection -> AccountId -> AccountId -> PostId -> UTCTime -> IO ()
notifyLike conn (AccountId author) (AccountId liker) (PostId post) now =
  void
    ( execute
        conn
        [sql|
          INSERT INTO notifications (recipient_id, actor_id, kind, post_id, created_at) VALUES (?, ?, 'liked', ?, ?)
          ON CONFLICT (actor_id, post_id) WHERE kind = 'liked' DO NOTHING
        |]
        (author, liker, post, now)
    )

-- | Rule CommentPublished: the parent's author is told, with the comment.
notifyComment :: Connection -> AccountId -> AccountId -> PostId -> PostId -> UTCTime -> IO ()
notifyComment conn (AccountId author) (AccountId commenter) (PostId parent) (PostId comment) now =
  void
    ( execute
        conn
        "INSERT INTO notifications (recipient_id, actor_id, kind, post_id, comment_id, created_at) VALUES (?, ?, 'commented', ?, ?, ?)"
        (author, commenter, parent, comment, now)
    )

forgetFollow :: Connection -> AccountId -> AccountId -> IO ()
forgetFollow conn (AccountId follower) (AccountId target) =
  void
    ( execute
        conn
        "DELETE FROM notifications WHERE kind IN ('followed', 'follow_requested') AND actor_id = ? AND recipient_id = ?"
        (follower, target)
    )

forgetLike :: Connection -> AccountId -> PostId -> IO ()
forgetLike conn (AccountId liker) (PostId post) =
  void (execute conn "DELETE FROM notifications WHERE kind = 'liked' AND actor_id = ? AND post_id = ?" (liker, post))

{- | Every notification about a post, and the one that reported it if it
is a comment.
-}
forgetPost :: Connection -> PostId -> IO ()
forgetPost conn (PostId post) =
  void (execute conn "DELETE FROM notifications WHERE post_id = ? OR comment_id = ?" (post, post))

-- | An account's notifications, newest first, below a cursor.
notificationsFor :: Connection -> AccountId -> Maybe Cursor -> Int -> IO [NotificationRow]
notificationsFor conn (AccountId recipient) cursor rows =
  query
    conn
    [sql|
      SELECT id, kind, actor_id, post_id, comment_id, created_at FROM notifications
      WHERE recipient_id = ? AND id < ? ORDER BY id DESC LIMIT ?
    |]
    (recipient, maybe maxBound (\(Cursor n) -> n) cursor, rows)

-- | Rule NotificationSeen, for the notifications a page showed.
markSeen :: Connection -> AccountId -> [Int64] -> IO ()
markSeen _ _ [] = pure ()
markSeen conn (AccountId recipient) ids =
  void (execute conn "UPDATE notifications SET status = 'seen' WHERE recipient_id = ? AND status = 'unread' AND id IN ?" (recipient, In ids))

-- | How many of an account's notifications no page has shown it yet.
unreadCount :: Connection -> AccountId -> IO Int
unreadCount conn (AccountId recipient) =
  maybe 0 fromOnly . listToMaybe <$> query conn "SELECT count(*) FROM notifications WHERE recipient_id = ? AND status = 'unread'" (Only recipient)
