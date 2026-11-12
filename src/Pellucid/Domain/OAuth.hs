{- | The decisions of the OAuth rules, with no database and no clock of
their own: each takes the facts and says what the rule allows. The store
carries the answer out inside one transaction.
-}
module Pellucid.Domain.OAuth (
  ClientKind (..),
  clientKindText,
  parseClientKind,
  Scope (..),
  Need (..),
  parseScopes,
  renderScopes,
  satisfies,
  GrantRequest (..),
  GrantRefusal (..),
  authorize,
  CodeStatus (..),
  parseCodeStatus,
  CodeFacts (..),
  Presentation (..),
  TokenRefusal (..),
  Redemption (..),
  redeem,
  TokenStatus (..),
  parseTokenStatus,
  tokenUsable,
) where

import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Pellucid.Domain.Account (AccountStatus, canSignIn)
import Pellucid.Domain.Pkce (Challenge, verifierMatches)

{- | A public client (an app on someone's device) cannot keep a secret and
proves itself with PKCE alone. A confidential client holds a secret.
-}
data ClientKind = PublicClient | ConfidentialClient
  deriving stock (Eq, Show, Enum, Bounded)

clientKindText :: ClientKind -> Text
clientKindText = \case
  PublicClient -> "public_client"
  ConfidentialClient -> "confidential_client"

parseClientKind :: Text -> Maybe ClientKind
parseClientKind = \case
  "public_client" -> Just PublicClient
  "confidential_client" -> Just ConfidentialClient
  _ -> Nothing

data Scope = ScopeRead | ScopeWrite | ScopeFollow | ScopePush
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | What a route asks of a token.
data Need = NeedRead | NeedWrite | NeedFollow
  deriving stock (Eq, Show, Enum, Bounded)

{- | The four scopes clients of this API ask for, separated by spaces. A
scope this server does not know is a refusal, not something to drop or
to widen: @admin:write@ must not turn into @write@.
-}
parseScopes :: Text -> Either Text (Set Scope)
parseScopes raw = Set.fromList <$> traverse one (T.words raw)
  where
    one = \case
      "read" -> Right ScopeRead
      "write" -> Right ScopeWrite
      "follow" -> Right ScopeFollow
      "push" -> Right ScopePush
      other -> Left other

renderScopes :: Set Scope -> Text
renderScopes = T.unwords . map name . Set.toAscList
  where
    name = \case
      ScopeRead -> "read"
      ScopeWrite -> "write"
      ScopeFollow -> "follow"
      ScopePush -> "push"

{- | @follow@ is the older name for the part of @write@ that changes who
follows whom, so either one opens those routes.
-}
satisfies :: Set Scope -> Need -> Bool
satisfies scopes = \case
  NeedRead -> ScopeRead `Set.member` scopes
  NeedWrite -> ScopeWrite `Set.member` scopes
  NeedFollow -> ScopeFollow `Set.member` scopes || ScopeWrite `Set.member` scopes

-- | What the consent page has in hand when the person presses Allow.
data GrantRequest = GrantRequest
  { accountStatus :: AccountStatus
  , clientKind :: ClientKind
  , registeredRedirects :: [Text]
  , registeredScopes :: Set Scope
  , redirectUri :: Text
  , scopes :: Set Scope
  , challenge :: Maybe Challenge
  }

data GrantRefusal
  = AccountMayNotSignIn
  | RedirectNotRegistered
  | ScopeNotRegistered
  | PublicClientWithoutPkce
  deriving stock (Eq, Show)

{- | Rule AuthorizationGranted. The redirect must be one the client
registered, character for character; a public client must bring a
challenge (invariant PublicClientsAlwaysUsePkce); and a client is given
no scope it did not register for.
-}
authorize :: GrantRequest -> Either GrantRefusal ()
authorize request
  | not (canSignIn request.accountStatus) = Left AccountMayNotSignIn
  | request.redirectUri `notElem` request.registeredRedirects = Left RedirectNotRegistered
  | not (request.scopes `Set.isSubsetOf` request.registeredScopes) = Left ScopeNotRegistered
  | request.clientKind == PublicClient && not (isJust request.challenge) = Left PublicClientWithoutPkce
  | otherwise = Right ()

data CodeStatus = Issued | Redeemed | Stale
  deriving stock (Eq, Show, Enum, Bounded)

parseCodeStatus :: Text -> Maybe CodeStatus
parseCodeStatus = \case
  "issued" -> Just Issued
  "redeemed" -> Just Redeemed
  "stale" -> Just Stale
  _ -> Nothing

-- | An authorization code as the store holds it, with what its client is.
data CodeFacts = CodeFacts
  { status :: CodeStatus
  , redirectUri :: Text
  , challenge :: Maybe Challenge
  , issuedAt :: UTCTime
  , clientKind :: ClientKind
  , clientSecretHash :: Maybe Text
  }

{- | What the token request brings with the code. The secret arrives
already hashed, so the plain secret never reaches this module.
-}
data Presentation = Presentation
  { redirectUri :: Text
  , verifier :: Maybe Text
  , secretHash :: Maybe Text
  }

data TokenRefusal
  = CodeStale
  | RedirectMismatch
  | VerifierMissing
  | VerifierWrong
  | SecretWrong
  deriving stock (Eq, Show)

data Redemption
  = -- | Mark the code redeemed and issue a token for it.
    IssueToken
  | -- | The code was redeemed before. One of the two presenters stole it,
    -- so the token it produced is revoked.
    RefuseAndRevoke
  | -- | The code outlived its lifetime before rule CodeExpired ran; the
    -- caller marks it stale.
    RefuseAndExpire
  | Refuse TokenRefusal
  deriving stock (Eq, Show)

{- | Rule TokenIssued, with rule CodeExpired folded in for a code whose
timer has not fired yet.
-}
redeem :: NominalDiffTime -> UTCTime -> CodeFacts -> Presentation -> Redemption
redeem lifetime now code presented
  | code.status == Redeemed = RefuseAndRevoke
  | code.status == Stale = Refuse CodeStale
  | addUTCTime lifetime code.issuedAt <= now = RefuseAndExpire
  | presented.redirectUri /= code.redirectUri = Refuse RedirectMismatch
  | otherwise = case pkce of
      Just refusal -> Refuse refusal
      Nothing
        | secretHolds -> IssueToken
        | otherwise -> Refuse SecretWrong
  where
    pkce = case (code.challenge, presented.verifier) of
      (Just challenge, Just verifier)
        | verifierMatches challenge verifier -> Nothing
        | otherwise -> Just VerifierWrong
      (Just _, Nothing) -> Just VerifierMissing
      -- A public client's code always has a challenge; one without is
      -- refused here as well, so the invariant does not rest on the
      -- consent page alone.
      (Nothing, _)
        | code.clientKind == PublicClient -> Just VerifierMissing
        | otherwise -> Nothing
    secretHolds = case code.clientKind of
      PublicClient -> True
      ConfidentialClient -> isJust code.clientSecretHash && presented.secretHash == code.clientSecretHash

data TokenStatus = Valid | Revoked | Lapsed
  deriving stock (Eq, Show, Enum, Bounded)

parseTokenStatus :: Text -> Maybe TokenStatus
parseTokenStatus = \case
  "valid" -> Just Valid
  "revoked" -> Just Revoked
  "lapsed" -> Just Lapsed
  _ -> Nothing

{- | A token speaks for its account while it is valid and younger than the
token lifetime (rule TokenExpired, checked at use so that it does not
depend on a timer having run).
-}
tokenUsable :: NominalDiffTime -> UTCTime -> TokenStatus -> UTCTime -> Bool
tokenUsable lifetime now status issuedAt = status == Valid && addUTCTime lifetime issuedAt > now
