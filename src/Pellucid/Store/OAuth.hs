{-# LANGUAGE QuasiQuotes #-}

{- | OAuth clients, authorization codes and access tokens. Secrets are
written and looked up by their hash only.
-}
module Pellucid.Store.OAuth (
  ClientRow (..),
  NewClient (..),
  insertClient,
  findClientByKey,
  NewCode (..),
  insertCode,
  TokenRequest (..),
  TokenGrant (..),
  TokenFailure (..),
  redeemCode,
  SessionRow (..),
  findSession,
  revokeToken,
) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Pellucid.Domain.Account (AccountStatus, parseAccountStatus)
import Pellucid.Domain.Ids (AccountId (..))
import Pellucid.Domain.OAuth (
  ClientKind,
  CodeFacts (..),
  Presentation,
  Redemption (..),
  Scope,
  TokenRefusal (..),
  TokenStatus,
  clientKindText,
  parseClientKind,
  parseCodeStatus,
  parseScopes,
  parseTokenStatus,
  redeem,
  renderScopes,
 )
import Pellucid.Domain.Pkce (Challenge, challengeText, mkChallenge)
import Pellucid.Store (parsed, transact)

data ClientRow = ClientRow
  { rowId :: Int64
  , clientKey :: Text
  , name :: Text
  , kind :: ClientKind
  , redirectUris :: [Text]
  , scopes :: Set Scope
  , secretHash :: Maybe Text
  }

instance FromRow ClientRow where
  fromRow =
    ClientRow
      <$> field
      <*> field
      <*> field
      <*> parsed parseClientKind
      <*> (fromPGArray <$> field)
      <*> parsed (either (const Nothing) Just . parseScopes)
      <*> field

data NewClient = NewClient
  { clientKey :: Text
  , name :: Text
  , kind :: ClientKind
  , redirectUris :: [Text]
  , scopes :: Set Scope
  , secretHash :: Maybe Text
  , website :: Maybe Text
  }

-- | Rule ClientRegistered.
insertClient :: Connection -> NewClient -> IO ()
insertClient conn client =
  void
    ( execute
        conn
        "INSERT INTO oauth_clients (client_key, name, kind, redirect_uris, scopes, secret_hash, website) VALUES (?, ?, ?, ?, ?, ?, ?)"
        ( client.clientKey
        , client.name
        , clientKindText client.kind
        , PGArray client.redirectUris
        , renderScopes client.scopes
        , client.secretHash
        , client.website
        )
    )

findClientByKey :: Connection -> Text -> IO (Maybe ClientRow)
findClientByKey conn key =
  listToMaybe
    <$> query conn "SELECT id, client_key, name, kind, redirect_uris, scopes, secret_hash FROM oauth_clients WHERE client_key = ?" (Only key)

data NewCode = NewCode
  { codeHash :: Text
  , client :: Int64
  , account :: AccountId
  , redirectUri :: Text
  , scopes :: Set Scope
  , challenge :: Maybe Challenge
  , issuedAt :: UTCTime
  }

-- | Rule AuthorizationGranted's effect; the decision is 'authorize'.
insertCode :: Connection -> NewCode -> IO ()
insertCode conn code =
  let AccountId account = code.account
   in void
        ( execute
            conn
            "INSERT INTO authorization_codes (code_hash, client_id, account_id, redirect_uri, scopes, pkce_challenge, issued_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
            (code.codeHash, code.client, account, code.redirectUri, renderScopes code.scopes, challengeText <$> code.challenge, code.issuedAt)
        )

-- | A token request, with the code and the new token already hashed.
data TokenRequest = TokenRequest
  { codeHash :: Text
  , clientKey :: Text
  , presentation :: Presentation
  , newTokenHash :: Text
  }

data TokenGrant = TokenGrant
  { scopes :: Text
  , issuedAt :: UTCTime
  }

data TokenFailure
  = -- | No such code, or it belongs to another client.
    UnknownCode
  | Refused TokenRefusal
  | -- | A redeemed code came back; the token it produced was revoked.
    Replayed

{- | Rule TokenIssued, in one transaction with the code's row locked, so
that two requests carrying the same code cannot both be answered with a
token. The outcomes that change state while refusing (a stale code, a
replay) are committed; the others change nothing.
-}
redeemCode :: Connection -> NominalDiffTime -> UTCTime -> TokenRequest -> IO (Either TokenFailure TokenGrant)
redeemCode conn lifetime now request = do
  -- The transaction's own result is always 'Right', so it always commits:
  -- the refusals that mark a code stale or revoke a token must be kept.
  outcome <- transact conn do
    rows <-
      query
        conn
        [sql|
          SELECT c.id, c.client_id, c.account_id, c.scopes, c.status, c.redirect_uri, c.pkce_challenge, c.issued_at,
                 k.kind, k.secret_hash
          FROM authorization_codes c JOIN oauth_clients k ON k.id = c.client_id
          WHERE c.code_hash = ? AND k.client_key = ?
          FOR UPDATE OF c
        |]
        (request.codeHash, request.clientKey)
    case rows of
      [] -> pure (Right (Left UnknownCode))
      (LockedCode {codeId, clientId, accountId, scopes, facts}) : _ ->
        case redeem lifetime now facts request.presentation of
          IssueToken -> do
            void (execute conn "UPDATE authorization_codes SET status = 'redeemed' WHERE id = ? AND status = 'issued'" (Only codeId))
            void
              ( execute
                  conn
                  "INSERT INTO access_tokens (token_hash, client_id, account_id, code_id, scopes, issued_at) VALUES (?, ?, ?, ?, ?, ?)"
                  (request.newTokenHash, clientId, accountId, codeId, scopes, now)
              )
            pure (Right (Right TokenGrant {scopes, issuedAt = now}))
          RefuseAndRevoke -> do
            void (execute conn "UPDATE access_tokens SET status = 'revoked' WHERE code_id = ? AND status = 'valid'" (Only codeId))
            pure (Right (Left Replayed))
          RefuseAndExpire -> do
            void (execute conn "UPDATE authorization_codes SET status = 'stale' WHERE id = ? AND status = 'issued'" (Only codeId))
            pure (Right (Left (Refused CodeStale)))
          Refuse refusal -> pure (Right (Left (Refused refusal)))
  pure (either (\() -> Left UnknownCode) id outcome)

-- | A code row as 'redeemCode' reads it under lock.
data LockedCode = LockedCode
  { codeId :: Int64
  , clientId :: Int64
  , accountId :: Int64
  , scopes :: Text
  , facts :: CodeFacts
  }

instance FromRow LockedCode where
  fromRow = do
    codeId <- field
    clientId <- field
    accountId <- field
    scopes <- field
    status <- parsed parseCodeStatus
    redirectUri <- field
    challenge <- field
    issuedAt <- field
    clientKind <- parsed parseClientKind
    clientSecretHash <- field
    pure
      LockedCode
        { codeId
        , clientId
        , accountId
        , scopes
        , facts =
            CodeFacts
              { status
              , redirectUri
              , -- A stored challenge was validated when it was written.
                challenge = challenge >>= \c -> either (const Nothing) Just (mkChallenge c "S256")
              , issuedAt
              , clientKind
              , clientSecretHash
              }
        }

-- | A token with the account it speaks for.
data SessionRow = SessionRow
  { account :: AccountId
  , accountStatus :: AccountStatus
  , isModerator :: Bool
  , scopes :: Set Scope
  , tokenStatus :: TokenStatus
  , issuedAt :: UTCTime
  }

instance FromRow SessionRow where
  fromRow =
    SessionRow
      <$> (AccountId <$> field)
      <*> parsed parseAccountStatus
      <*> field
      <*> parsed (either (const Nothing) Just . parseScopes)
      <*> parsed parseTokenStatus
      <*> field

findSession :: Connection -> Text -> IO (Maybe SessionRow)
findSession conn tokenHash =
  listToMaybe
    <$> query
      conn
      [sql|
        SELECT a.id, a.status, a.is_moderator, t.scopes, t.status, t.issued_at
        FROM access_tokens t JOIN accounts a ON a.id = t.account_id
        WHERE t.token_hash = ?
      |]
      (Only tokenHash)

{- | Rule TokenRevoked. Whoever holds the token can give it back (S11);
revoking a token that is unknown or already revoked changes nothing.
-}
revokeToken :: Connection -> Text -> IO ()
revokeToken conn tokenHash =
  void (execute conn "UPDATE access_tokens SET status = 'revoked' WHERE token_hash = ? AND status = 'valid'" (Only tokenHash))
