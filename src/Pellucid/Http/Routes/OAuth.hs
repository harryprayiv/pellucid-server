{- | Surface SignIn: registering a client, the consent page, the token
endpoint and giving a token back.

The consent page keeps no session. It asks for the password each time
and carries the authorization request in hidden fields, every one of
which is checked again when the form comes in. A forged form from
another site therefore gets nowhere: it would need the password.
-}
module Pellucid.Http.Routes.OAuth (
  routes,
  validRedirect,
) where

import Control.Exception (evaluate)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (Value (..), object, (.=))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (Status, hLocation, methodGet, methodPost, status200, status302, status400, status401, urlEncode)
import Network.Wai qualified as Wai
import Pellucid.Config (Config (..), Env (..))
import Pellucid.Domain.Limits (Limits (..))
import Pellucid.Domain.OAuth (
  ClientKind (..),
  GrantRefusal (..),
  GrantRequest (..),
  Presentation (..),
  Scope,
  TokenRefusal (..),
  authorize,
  parseScopes,
  renderScopes,
 )
import Pellucid.Domain.Pkce (Challenge, PkceProblem (..), mkChallenge)
import Pellucid.Http.Pages (SignIn (..), codePage, html, messagePage, signInPage)
import Pellucid.Http.Params (BodyLimit (..), FieldProblem (..), Parser, boundedText, optionalField, required, runParser, text, withDefault)
import Pellucid.Http.Respond (Handler, Stop (..), invalid, json, oauthError, safetyHeaders)
import Pellucid.Http.Router (Access (..), Request (..), Route, parse, route)
import Pellucid.Secret (hashSecret, newSecret, passwordMatches)
import Pellucid.Store (withConnection)
import Pellucid.Store.Accounts (AccountRow (..), findLocalByUsername)
import Pellucid.Store.OAuth (
  ClientRow (..),
  NewClient (..),
  NewCode (..),
  TokenFailure (..),
  TokenGrant (..),
  TokenRequest (..),
  findClientByKey,
  insertClient,
  insertCode,
  redeemCode,
  revokeToken,
 )

routes :: [Route]
routes =
  [ route methodPost "/api/v1/apps" Open SmallBody registerApp
  , route methodGet "/oauth/authorize" Open SmallBody consentPage
  , route methodPost "/oauth/authorize" Open SmallBody consentGiven
  , route methodPost "/oauth/token" Open SmallBody token
  , route methodPost "/oauth/revoke" Open SmallBody revoke
  ]

-- Registration ------------------------------------------------------------------

{- | Rule ClientRegistered. A client is confidential and is given a secret
unless it registers with @token_endpoint_auth_method=none@ (RFC 7591's
name for a client that cannot keep one), which makes it a public client
that must use PKCE. Existing clients of this API send no such field and
expect a secret back.
-}
registerApp :: Env -> Request -> Handler Wai.Response
registerApp env request = do
  (name, redirects, scopeText, website, authMethod) <-
    parse request $
      (,,,,)
        <$> required "client_name" (boundedText 100)
        <*> required "redirect_uris" (boundedText 4000)
        <*> withDefault "read" "scopes" (boundedText 200)
        <*> optionalField "website" (boundedText 2000)
        <*> optionalField "token_endpoint_auth_method" text
  let
    redirectUris = T.words redirects
    problems =
      [FieldProblem "client_name" "is empty" | T.null (T.strip name)]
        <> [FieldProblem "redirect_uris" "names no redirect" | null redirectUris]
        <> [FieldProblem "redirect_uris" "names more than ten redirects" | length redirectUris > 10]
        <> [FieldProblem "redirect_uris" (uri <> " " <> why) | uri <- redirectUris, Just why <- [validRedirect uri]]
  scopes <- case parseScopes scopeText of
    Right scopes | not (Set.null scopes) -> pure scopes
    Right _ -> throwE (invalid (FieldProblem "scopes" "names no scope" : problems))
    Left unknown -> throwE (invalid (FieldProblem "scopes" ("names a scope this server does not have: " <> unknown) : problems))
  unless (null problems) (throwE (invalid problems))
  let kind = if authMethod == Just "none" then PublicClient else ConfidentialClient
  clientKey <- liftIO newSecret
  secret <- case kind of
    PublicClient -> pure Nothing
    ConfidentialClient -> Just <$> liftIO newSecret
  liftIO . withConnection env.db $ \conn ->
    insertClient
      conn
      NewClient
        { clientKey
        , name = T.strip name
        , kind
        , redirectUris
        , scopes
        , secretHash = hashSecret <$> secret
        , website
        }
  pure . json status200 [] $
    object
      [ "id" .= clientKey
      , "name" .= T.strip name
      , "website" .= website
      , "redirect_uri" .= T.unwords redirectUris
      , "client_id" .= clientKey
      , "client_secret" .= secret
      , "vapid_key" .= ("" :: Text)
      ]

{- | Why a redirect cannot be registered, or 'Nothing' when it can. A
redirect is where the code is sent, so it is one of: an https URL, an
http URL on this device (an app listening on the loopback address), an
app's own scheme, or the out-of-band marker. It carries no fragment,
and an http or https one carries no user name, which is how a URL that
begins like one host can belong to another.
-}
validRedirect :: Text -> Maybe Text
validRedirect uri
  | uri == outOfBand = Nothing
  | T.any (== '#') uri = Just "has a fragment"
  | otherwise = case T.breakOn ":" uri of
      (scheme, rest)
        | T.null rest || T.null scheme || not (schemeShaped scheme) -> Just "is not an absolute URI"
        | lower scheme `elem` ["javascript", "data", "vbscript", "file", "blob", "about"] -> Just "uses a scheme that cannot receive a redirect"
        | lower scheme `elem` ["http", "https"] -> case T.stripPrefix "://" rest of
            Nothing -> Just "is not an absolute URI"
            Just afterSlashes ->
              let authority = T.takeWhile (\c -> c /= '/' && c /= '?') afterSlashes
               in if T.null authority
                    then Just "is not an absolute URI"
                    else
                      if T.any (== '@') authority
                        then Just "carries a user name"
                        else
                          if lower scheme == "https" || loopback (lower authority)
                            then Nothing
                            else Just "uses http for something other than this device; use https"
        | otherwise -> Nothing
  where
    lower = T.toLower
    schemeShaped scheme =
      maybe False (\(c, _) -> isAsciiLower c || isAsciiUpper c) (T.uncons scheme)
        && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ['+', '-', '.']) scheme
    -- A loopback host, alone or followed by a port.
    loopback authority =
      any
        ( \host -> case T.stripPrefix host authority of
            Just "" -> True
            Just port -> maybe False (\digits -> not (T.null digits) && T.all isDigit digits) (T.stripPrefix ":" port)
            Nothing -> False
        )
        ["127.0.0.1", "localhost", "[::1]"]

outOfBand :: Text
outOfBand = "urn:ietf:wg:oauth:2.0:oob"

-- Consent -----------------------------------------------------------------------

{- | The authorization request, as the query of the GET and again as the
hidden fields of the POST.
-}
data Ask = Ask
  { responseType :: Text
  , clientKey :: Text
  , redirectUri :: Text
  , scope :: Maybe Text
  , state :: Maybe Text
  , challenge :: Maybe Text
  , method :: Maybe Text
  }

askParser :: Parser Ask
askParser =
  Ask
    <$> required "response_type" text
    <*> required "client_id" (boundedText 200)
    <*> required "redirect_uri" (boundedText 2000)
    <*> optionalField "scope" (boundedText 200)
    <*> optionalField "state" (boundedText 2000)
    <*> optionalField "code_challenge" (boundedText 200)
    <*> optionalField "code_challenge_method" (boundedText 20)

{- | An authorization request that has been checked against the client it
names. Until this exists nothing is sent to the redirect: a redirect
that is not the client's own is answered with a page, never followed.
-}
data Checked = Checked
  { ask :: Ask
  , client :: ClientRow
  , scopes :: Set Scope
  , pkce :: Maybe Challenge
  }

checkAsk :: Env -> Request -> Handler Checked
checkAsk env request = do
  ask <- either (\problems -> page status400 "This sign-in link is incomplete" (describe problems)) pure (runParser askParser request.params)
  found <- liftIO (withConnection env.db \conn -> findClientByKey conn ask.clientKey)
  client <- maybe (page status400 "Unknown app" "The app that sent you here is not registered with this server.") pure found
  unless (ask.redirectUri `elem` client.redirectUris) $
    page status400 "This sign-in link is not the app's own" "The address this link would send you back to is not one the app registered, so nothing was sent there."
  unless (ask.responseType == "code") $
    page status400 "Unsupported sign-in" "This server signs apps in with an authorization code only."
  pkce <- case (ask.challenge, client.kind) of
    (Just challenge, _) -> case mkChallenge challenge (fromMaybe "plain" ask.method) of
      Right ok -> pure (Just ok)
      Left MethodNotS256 -> page status400 "Unsupported sign-in" "The app must use the S256 code challenge method."
      Left ChallengeMalformed -> page status400 "This sign-in link is damaged" "The code challenge in it is not a valid one."
    (Nothing, PublicClient) -> page status400 "Unsupported sign-in" "This app has no secret, so it must send a code challenge (PKCE)."
    (Nothing, ConfidentialClient) -> pure Nothing
  scopes <- case parseScopes (fromMaybe (renderScopes client.scopes) ask.scope) of
    Right asked | asked `Set.isSubsetOf` client.scopes && not (Set.null asked) -> pure asked
    _ -> page status400 "The app asked for more than it registered for" "Nothing was granted."
  pure Checked {ask, client, scopes, pkce}
  where
    describe problems = T.intercalate "; " [problem.field <> " " <> problem.problem | problem <- problems]

-- | Stop with a page for the person at the browser.
page :: Status -> Text -> Text -> Handler a
page status heading body = throwE (Answer (html status (messagePage heading body)))

consentPage :: Env -> Request -> Handler Wai.Response
consentPage env request = do
  checked <- checkAsk env request
  pure (html status200 (signInPage (viewOf env checked "" Nothing)))

viewOf :: Env -> Checked -> Text -> Maybe Text -> SignIn
viewOf env checked username problem =
  SignIn
    { serverTitle = env.config.title
    , appName = checked.client.name
    , scopes = T.words (renderScopes checked.scopes)
    , carried =
        [ ("response_type", checked.ask.responseType)
        , ("client_id", checked.ask.clientKey)
        , ("redirect_uri", checked.ask.redirectUri)
        , ("scope", renderScopes checked.scopes)
        ]
          <> [("state", value) | Just value <- [checked.ask.state]]
          <> [("code_challenge", value) | Just value <- [checked.ask.challenge]]
          <> [("code_challenge_method", value) | Just value <- [checked.ask.method]]
    , username
    , problem
    }

-- | Rule AuthorizationGranted.
consentGiven :: Env -> Request -> Handler Wai.Response
consentGiven env request = do
  checked <- checkAsk env request
  (decision, username, password) <-
    parse request ((,,) <$> withDefault "allow" "decision" text <*> withDefault "" "username" (boundedText 200) <*> withDefault "" "password" (boundedText 1000))
  if decision /= "allow"
    then pure (sendBack checked [("error", "access_denied"), ("error_description", "the person declined")])
    else do
      found <- liftIO (withConnection env.db \conn -> findLocalByUsername conn (T.toLower (T.strip username)))
      -- One password check is paid for whether or not the name exists.
      matches <- liftIO (evaluate (passwordMatches password (maybe env.decoyPasswordHash snd found)))
      case found of
        Just (account, _)
          | matches -> do
              let grant =
                    GrantRequest
                      { accountStatus = account.status
                      , clientKind = checked.client.kind
                      , registeredRedirects = checked.client.redirectUris
                      , registeredScopes = checked.client.scopes
                      , redirectUri = checked.ask.redirectUri
                      , scopes = checked.scopes
                      , challenge = checked.pkce
                      }
              case authorize grant of
                Left AccountMayNotSignIn -> pure (html status401 (signInPage (viewOf env checked username (Just "This account can no longer sign in."))))
                Left _ -> pure (html status400 (messagePage "Nothing was granted" "The request did not match what the app registered."))
                Right () -> do
                  code <- liftIO newSecret
                  now <- liftIO env.clock
                  liftIO . withConnection env.db $ \conn ->
                    insertCode
                      conn
                      NewCode
                        { codeHash = hashSecret code
                        , client = checked.client.rowId
                        , account = account.accountId
                        , redirectUri = checked.ask.redirectUri
                        , scopes = checked.scopes
                        , challenge = checked.pkce
                        , issuedAt = now
                        }
                  pure
                    ( if checked.ask.redirectUri == outOfBand
                        then html status200 (codePage code)
                        else sendBack checked [("code", code)]
                    )
        _ -> pure (html status401 (signInPage (viewOf env checked username (Just "That username and password do not match an account here."))))

{- | Send the browser back to the client's redirect with the given query
added and the client's @state@ returned untouched.
-}
sendBack :: Checked -> [(Text, Text)] -> Wai.Response
sendBack checked pairs =
  let
    base = checked.ask.redirectUri
    joiner = if T.any (== '?') base then "&" else "?"
    query = T.intercalate "&" [name <> "=" <> encoded value | (name, value) <- pairs <> [("state", value) | Just value <- [checked.ask.state]]]
    encoded = TE.decodeUtf8 . urlEncode True . TE.encodeUtf8
   in
    Wai.responseLBS status302 ((hLocation, TE.encodeUtf8 (base <> joiner <> query)) : safetyHeaders) ""

-- Tokens ------------------------------------------------------------------------

-- | Rule TokenIssued.
token :: Env -> Request -> Handler Wai.Response
token env request = do
  (grantType, code, clientKey, redirectUri, secret, verifier) <-
    oauthParse request $
      (,,,,,)
        <$> required "grant_type" text
        <*> required "code" (boundedText 500)
        <*> required "client_id" (boundedText 200)
        <*> required "redirect_uri" (boundedText 2000)
        <*> optionalField "client_secret" (boundedText 500)
        <*> optionalField "code_verifier" (boundedText 200)
  when (grantType /= "authorization_code") $
    throwE (oauthError "unsupported_grant_type" "this server issues tokens for an authorization code only")
  tokenText <- liftIO newSecret
  now <- liftIO env.clock
  let
    lifetime = env.config.limits.accessTokenLifetime
    asked =
      TokenRequest
        { codeHash = hashSecret code
        , clientKey
        , presentation = Presentation {redirectUri, verifier, secretHash = hashSecret <$> secret}
        , newTokenHash = hashSecret tokenText
        }
  result <- liftIO (withConnection env.db \conn -> redeemCode conn env.config.limits.authorizationCodeLifetime now asked)
  case result of
    Right grant ->
      pure . json status200 [] $
        object
          [ "access_token" .= tokenText
          , "token_type" .= ("Bearer" :: Text)
          , "scope" .= grant.scopes
          , "created_at" .= (floor (utcTimeToPOSIXSeconds grant.issuedAt) :: Integer)
          , "expires_in" .= (floor lifetime :: Integer)
          ]
    Left UnknownCode -> throwE (oauthError "invalid_grant" "the code is not one this server issued to this client")
    Left Replayed -> throwE (oauthError "invalid_grant" "the code was used before; the token it produced has been revoked")
    Left (Refused refusal) -> throwE case refusal of
      CodeStale -> oauthError "invalid_grant" "the code has expired"
      RedirectMismatch -> oauthError "invalid_grant" "redirect_uri is not the one the code was issued for"
      VerifierMissing -> oauthError "invalid_grant" "code_verifier is required for this code"
      VerifierWrong -> oauthError "invalid_grant" "code_verifier does not match the code challenge"
      SecretWrong -> oauthError "invalid_client" "client_secret is missing or wrong"

{- | Rule TokenRevoked, as RFC 7009 has it: the answer is the same whether
or not the token existed.
-}
revoke :: Env -> Request -> Handler Wai.Response
revoke env request = do
  tokenText <- oauthParse request (required "token" (boundedText 500))
  liftIO (withConnection env.db \conn -> revokeToken conn (hashSecret tokenText))
  pure (json status200 [] (Object mempty))

-- | Like 'parse', with the refusal in the shape RFC 6749 gives it.
oauthParse :: Request -> Parser a -> Handler a
oauthParse request parser = case runParser parser request.params of
  Right value -> pure value
  Left problems -> throwE (oauthError "invalid_request" (T.intercalate "; " [problem.field <> " " <> problem.problem | problem <- problems]))
