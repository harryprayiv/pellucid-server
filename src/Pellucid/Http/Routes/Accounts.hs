-- | Accounts, their posts, and following.
module Pellucid.Http.Routes.Accounts (
  routes,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (Value)
import Network.HTTP.Types (methodGet, methodPost, status200, status403, status422)
import Network.Wai qualified as Wai
import Pellucid.Config (Env (..), absoluteUrl)
import Pellucid.Domain.Account (isMember)
import Pellucid.Domain.Cursor (Page (..), paginate)
import Pellucid.Domain.Follow (FollowOutcome (..), FollowRefusal (..), FollowRequest (..), FollowState (..), decideFollow)
import Pellucid.Domain.Ids (AccountId (..), PostId (..), idText, parseId)
import Pellucid.Domain.OAuth (Need (..))
import Pellucid.Domain.Post (Viewer (..))
import Pellucid.Http.Entities (accountJson, loadStatuses, relationshipJson, viewerOf)
import Pellucid.Http.Params (BodyLimit (..), listOf, text)
import Pellucid.Http.Respond (Handler, json, notFound, pageLink, refuse)
import Pellucid.Http.Router (Access (..), Request (..), Route, Session (..), capture, parse, requireViewer, route)
import Pellucid.Http.Routes.Statuses (listWindow)
import Pellucid.Store (withConnection)
import Pellucid.Store.Accounts (AccountRow (..), deleteFollow, findAccount, followState, followsBetween, insertFollow)
import Pellucid.Store.Posts (PostRow (..), accountTimeline)

-- | The two literal paths come before the pattern that would match them.
routes :: [Route]
routes =
  [ route methodGet "/api/v1/accounts/verify_credentials" (Signed NeedRead) SmallBody verifyCredentials
  , route methodGet "/api/v1/accounts/relationships" (Signed NeedRead) SmallBody relationships
  , route methodGet "/api/v1/accounts/:id" Open SmallBody showAccount
  , route methodGet "/api/v1/accounts/:id/statuses" Open SmallBody accountStatuses
  , route methodPost "/api/v1/accounts/:id/follow" (Signed NeedFollow) SmallBody follow
  , route methodPost "/api/v1/accounts/:id/unfollow" (Signed NeedFollow) SmallBody unfollow
  ]

verifyCredentials :: Env -> Request -> Handler Wai.Response
verifyCredentials env request = do
  session <- requireViewer request
  account <- accountOrMissing env session.account
  pure (json status200 [] (accountJson env.config account))

showAccount :: Env -> Request -> Handler Wai.Response
showAccount env request = do
  account <- capturedAccount env request
  pure (json status200 [] (accountJson env.config account))

{- | An account's posts, as the caller may see them: followers-only posts
are in the list for the author and for an accepted follower.
-}
accountStatuses :: Env -> Request -> Handler Wai.Response
accountStatuses env request = do
  account <- capturedAccount env request
  (cursor, limit) <- listWindow env request
  (page, entities) <- liftIO . withConnection env.db $ \conn -> do
    viewer <- viewerOf conn request.viewer account.accountId
    rows <- accountTimeline conn account.accountId (viewer.isAuthor || viewer.isAcceptedFollower) cursor (limit + 1)
    let page = paginate limit (\row -> let PostId n = row.postId in n) rows
    entities <- loadStatuses env.config conn page.items
    pure (page, entities)
  let
    AccountId n = account.accountId
    base = absoluteUrl env.config ("/api/v1/accounts/" <> idText n <> "/statuses?limit=" <> idText (fromIntegral limit))
  pure (json status200 (pageLink base page.next) (entities :: [Value]))

relationships :: Env -> Request -> Handler Wai.Response
relationships env request = do
  session <- requireViewer request
  raw <- parse request (listOf "id" text)
  let others = [AccountId n | Just n <- map parseId raw]
  rows <- liftIO (withConnection env.db \conn -> followsBetween conn session.account others)
  pure (json status200 [] (map relationshipJson rows))

-- | Rules FollowedOpenAccount and FollowRequestedOfLockedAccount.
follow :: Env -> Request -> Handler Wai.Response
follow env request = do
  session <- requireViewer request
  target <- capturedAccount env request
  outcome <- liftIO . withConnection env.db $ \conn -> do
    standing <- followState conn session.account target.accountId
    let decision =
          decideFollow
            FollowRequest
              { followerStatus = session.accountStatus
              , targetStatus = target.status
              , sameAccount = session.account == target.accountId
              , targetLocked = target.locked
              , existing = standing
              }
    case decision of
      Right StartFollowing -> insertFollow conn session.account target.accountId Following
      Right StartRequest -> insertFollow conn session.account target.accountId Requested
      _ -> pure ()
    pure decision
  case outcome of
    Left CannotFollowSelf -> throwE (refuse status422 "cannot_follow_self" "an account cannot follow itself")
    Left FollowerNotActive -> throwE (refuse status403 "account_not_active" "this account cannot follow anyone while it is suspended or leaving")
    Left TargetNotActive -> throwE notFound
    Right _ -> relationshipWith env session target.accountId

-- | Rule Unfollowed.
unfollow :: Env -> Request -> Handler Wai.Response
unfollow env request = do
  session <- requireViewer request
  target <- capturedAccount env request
  liftIO (withConnection env.db \conn -> deleteFollow conn session.account target.accountId)
  relationshipWith env session target.accountId

relationshipWith :: Env -> Session -> AccountId -> Handler Wai.Response
relationshipWith env session other = do
  rows <- liftIO (withConnection env.db \conn -> followsBetween conn session.account [other])
  case rows of
    row : _ -> pure (json status200 [] (relationshipJson row))
    [] -> throwE notFound

{- | The account a path names. An id that is not a number, an account that
does not exist and an account that has departed are all the same
missing page.
-}
capturedAccount :: Env -> Request -> Handler AccountRow
capturedAccount env request = do
  raw <- capture 0 request
  case parseId raw of
    Just n -> accountOrMissing env (AccountId n)
    Nothing -> throwE notFound

accountOrMissing :: Env -> AccountId -> Handler AccountRow
accountOrMissing env account = do
  found <- liftIO (withConnection env.db \conn -> findAccount conn account)
  case found of
    Just row | isMember row.status -> pure row
    _ -> throwE notFound
