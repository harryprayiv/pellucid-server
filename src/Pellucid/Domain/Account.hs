-- | An account's lifecycle, as entity Account in the spec declares it.
module Pellucid.Domain.Account (
  AccountStatus (..),
  accountStatusText,
  parseAccountStatus,
  accountNext,
  canSignIn,
  isMember,
  canAct,
) where

import Data.Text (Text)

data AccountStatus = Active | Suspended | Leaving | Departed
  deriving stock (Eq, Show, Enum, Bounded)

accountStatusText :: AccountStatus -> Text
accountStatusText = \case
  Active -> "active"
  Suspended -> "suspended"
  Leaving -> "leaving"
  Departed -> "departed"

parseAccountStatus :: Text -> Maybe AccountStatus
parseAccountStatus = \case
  "active" -> Just Active
  "suspended" -> Just Suspended
  "leaving" -> Just Leaving
  "departed" -> Just Departed
  _ -> Nothing

-- | The transition graph. 'Departed' is terminal.
accountNext :: AccountStatus -> [AccountStatus]
accountNext = \case
  Active -> [Suspended, Leaving]
  Suspended -> [Active, Leaving]
  Leaving -> [Active, Departed]
  Departed -> []

{- | Rule AuthorizationGranted: an active or a suspended account can give
consent. A suspended one needs a session to read its notices, appeal,
export and leave (S8, S9).
-}
canSignIn :: AccountStatus -> Bool
canSignIn status = status == Active || status == Suspended

{- | Actor Member: who a token can speak for. An account that is leaving
keeps its session so that it can withdraw the deletion.
-}
isMember :: AccountStatus -> Bool
isMember status = status /= Departed

{- | What surface ClientApi guards with @when member.status = active@:
posting, commenting, liking, sharing, following, messaging, reporting.
-}
canAct :: AccountStatus -> Bool
canAct status = status == Active
