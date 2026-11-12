{- | Rules FollowedOpenAccount and FollowRequestedOfLockedAccount as one
decision.
-}
module Pellucid.Domain.Follow (
  FollowState (..),
  parseFollowState,
  FollowRequest (..),
  FollowRefusal (..),
  FollowOutcome (..),
  decideFollow,
) where

import Data.Text (Text)
import Pellucid.Domain.Account (AccountStatus, canAct)

-- | What already stands between the two accounts.
data FollowState = NoFollow | Requested | Following
  deriving stock (Eq, Show, Enum, Bounded)

parseFollowState :: Text -> Maybe FollowState
parseFollowState = \case
  "requested" -> Just Requested
  "following" -> Just Following
  _ -> Nothing

data FollowRequest = FollowRequest
  { followerStatus :: AccountStatus
  , targetStatus :: AccountStatus
  , sameAccount :: Bool
  , targetLocked :: Bool
  , existing :: FollowState
  }
  deriving stock (Eq, Show)

data FollowRefusal
  = CannotFollowSelf
  | FollowerNotActive
  | TargetNotActive
  deriving stock (Eq, Show)

data FollowOutcome
  = -- | Create the follow as following.
    StartFollowing
  | -- | Create the follow as requested; the target decides.
    StartRequest
  | -- | Something already stands; nothing changes and the answer is the
    -- relationship as it is. Asking twice is not an error in this API.
    AlreadyStands FollowState
  deriving stock (Eq, Show)

{- | Invariants NobodyFollowsThemselves and OneFollowPerPair are the first
and the last line. Blocks are not consulted until rule AccountBlocked is
built.
-}
decideFollow :: FollowRequest -> Either FollowRefusal FollowOutcome
decideFollow request
  | request.sameAccount = Left CannotFollowSelf
  | not (canAct request.followerStatus) = Left FollowerNotActive
  | not (canAct request.targetStatus) = Left TargetNotActive
  | request.existing /= NoFollow = Right (AlreadyStands request.existing)
  | request.targetLocked = Right StartRequest
  | otherwise = Right StartFollowing
