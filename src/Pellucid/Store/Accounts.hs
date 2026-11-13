{-# LANGUAGE QuasiQuotes #-}

-- | Accounts and the follows between them.
module Pellucid.Store.Accounts (
  AccountRow (..),
  createLocalAccount,
  findAccount,
  findAccounts,
  findLocalByUsername,
  countLocalAccounts,
  FollowRow (..),
  followState,
  insertFollow,
  deleteFollow,
  followsBetween,
) where

import Control.Monad (void, when)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), Query, execute, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pellucid.Domain.Account (AccountStatus, parseAccountStatus)
import Pellucid.Domain.Follow (FollowState (..), parseFollowState)
import Pellucid.Domain.Ids (AccountId (..))
import Pellucid.Domain.Notification (NotificationKind (..))
import Pellucid.Domain.Username (Username, usernameKey, usernameText)
import Pellucid.Store (parsed, transact)
import Pellucid.Store.Notifications (forgetFollow, notifyFollow)

-- | An account with the three counts every entity that shows it carries.
data AccountRow = AccountRow
  { accountId :: AccountId
  , username :: Text
  , home :: Maybe Text
  , displayName :: Text
  , note :: Text
  , locked :: Bool
  , isModerator :: Bool
  , status :: AccountStatus
  , createdAt :: UTCTime
  , followersCount :: Int
  , followingCount :: Int
  , postsCount :: Int
  }

instance FromRow AccountRow where
  fromRow =
    AccountRow
      <$> (AccountId <$> field)
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> parsed parseAccountStatus
      <*> field
      <*> field
      <*> field
      <*> field

{- | Followers and following count accepted follows only; posts counts
published posts that are not comments.
-}
selectAccount :: Query
selectAccount =
  [sql|
    SELECT a.id, a.username, a.home, a.display_name, a.note, a.locked, a.is_moderator, a.status, a.created_at,
      (SELECT count(*) FROM follows f WHERE f.target_id = a.id AND f.status = 'following'),
      (SELECT count(*) FROM follows f WHERE f.follower_id = a.id AND f.status = 'following'),
      (SELECT count(*) FROM posts p WHERE p.author_id = a.id AND p.status = 'published' AND p.in_reply_to IS NULL)
    FROM accounts a
  |]

{- | Rule AccountRegistered, for an operator: registration through the API
is closed. 'Nothing' when the name is taken, compared without case.
-}
createLocalAccount :: Connection -> Username -> Text -> Bool -> IO (Maybe AccountId)
createLocalAccount conn username passwordHash moderator = do
  rows <-
    query
      conn
      [sql|
        INSERT INTO accounts (username, username_key, display_name, password_hash, is_moderator)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT (username_key) WHERE home IS NULL DO NOTHING
        RETURNING id
      |]
      (usernameText username, usernameKey username, usernameText username, passwordHash, moderator)
  pure (AccountId . fromOnly <$> listToMaybe rows)

findAccount :: Connection -> AccountId -> IO (Maybe AccountRow)
findAccount conn (AccountId n) = listToMaybe <$> query conn (selectAccount <> " WHERE a.id = ?") (Only n)

findAccounts :: Connection -> [AccountId] -> IO [AccountRow]
findAccounts _ [] = pure []
findAccounts conn ids = query conn (selectAccount <> " WHERE a.id IN ?") (Only (In [n | AccountId n <- ids]))

-- | A local account by the key of its username, with its password hash.
findLocalByUsername :: Connection -> Text -> IO (Maybe (AccountRow, Text))
findLocalByUsername conn key = do
  hashes <- query conn "SELECT id, password_hash FROM accounts WHERE home IS NULL AND username_key = ? AND password_hash IS NOT NULL" (Only key)
  case hashes of
    (n, hash) : _ -> fmap (,hash) <$> findAccount conn (AccountId n)
    [] -> pure Nothing

countLocalAccounts :: Connection -> IO Int
countLocalAccounts conn = maybe 0 fromOnly . listToMaybe <$> query_ conn "SELECT count(*) FROM accounts WHERE home IS NULL AND status <> 'departed'"

{- | What stands between a viewer and one other account, in both
directions.
-}
data FollowRow = FollowRow
  { other :: AccountId
  , outgoing :: FollowState
  , incoming :: FollowState
  }

followState :: Connection -> AccountId -> AccountId -> IO FollowState
followState conn (AccountId follower) (AccountId target) = do
  rows <- query conn "SELECT status FROM follows WHERE follower_id = ? AND target_id = ?" (follower, target)
  pure (maybe NoFollow (maybe NoFollow id . parseFollowState . fromOnly) (listToMaybe rows))

{- | Insert a follow in the given state and tell the target, in one
transaction. The primary key makes a second insert for the same pair a
no-op (invariant OneFollowPerPair), and then nobody is told again.
-}
insertFollow :: Connection -> AccountId -> AccountId -> FollowState -> UTCTime -> IO ()
insertFollow conn follower target state now = case state of
  NoFollow -> pure ()
  Requested -> go "requested" FollowRequested
  Following -> go "following" Followed
  where
    AccountId followerNumber = follower
    AccountId targetNumber = target
    go :: Text -> NotificationKind -> IO ()
    go status kind = void . transact conn $ do
      created <-
        execute
          conn
          "INSERT INTO follows (follower_id, target_id, status) VALUES (?, ?, ?) ON CONFLICT (follower_id, target_id) DO NOTHING"
          (followerNumber, targetNumber, status)
      when (created == 1) (notifyFollow conn kind follower target now)
      pure (Right () :: Either () ())

{- | Rule Unfollowed. Also how a pending request is withdrawn. The notice
of the follow goes with the follow (decision S20).
-}
deleteFollow :: Connection -> AccountId -> AccountId -> IO ()
deleteFollow conn follower target = void . transact conn $ do
  void (execute conn "DELETE FROM follows WHERE follower_id = ? AND target_id = ?" (followerNumber, targetNumber))
  forgetFollow conn follower target
  pure (Right () :: Either () ())
  where
    AccountId followerNumber = follower
    AccountId targetNumber = target

-- | The follows between a viewer and each of the given accounts.
followsBetween :: Connection -> AccountId -> [AccountId] -> IO [FollowRow]
followsBetween _ _ [] = pure []
followsBetween conn (AccountId viewer) others = do
  let ids = [n | AccountId n <- others]
  out <- query conn "SELECT target_id, status FROM follows WHERE follower_id = ? AND target_id IN ?" (viewer, In ids) :: IO [(Int64, Text)]
  inc <- query conn "SELECT follower_id, status FROM follows WHERE target_id = ? AND follower_id IN ?" (viewer, In ids) :: IO [(Int64, Text)]
  let stateIn rows n = maybe NoFollow (maybe NoFollow id . parseFollowState) (lookup n rows)
  pure [FollowRow {other = AccountId n, outgoing = stateIn out n, incoming = stateIn inc n} | n <- ids]
