{- | The WAI application: find the route, find out who is calling, check
that the route is open to them, read the inputs, run the handler.
-}
module Pellucid.Http.App (
  application,
  routeTable,
) where

import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Types (hAuthorization, methodOptions, status204, status400, status403, status405, status500)
import Network.Wai qualified as Wai
import Pellucid.Config (Config (..), Env (..))
import Pellucid.Domain.Account (isMember)
import Pellucid.Domain.Limits (Limits (..))
import Pellucid.Domain.OAuth (satisfies, tokenUsable)
import Pellucid.Http.Params (readParams)
import Pellucid.Http.Respond (Stop, notFound, refuse, safetyHeaders, stopResponse, unauthorized)
import Pellucid.Http.Router (Access (..), Dispatch (..), Request (..), Route (..), Session (..), dispatch)
import Pellucid.Http.Routes.Accounts qualified as Accounts
import Pellucid.Http.Routes.Instance qualified as Instance
import Pellucid.Http.Routes.Media qualified as Media
import Pellucid.Http.Routes.OAuth qualified as OAuth
import Pellucid.Http.Routes.Statuses qualified as Statuses
import Pellucid.Secret (hashSecret)
import Pellucid.Store (withConnection)
import Pellucid.Store.OAuth (SessionRow (..), findSession)
import System.IO (hPutStrLn, stderr)

-- | Every route this server answers.
routeTable :: Limits -> [Route]
routeTable limits =
  concat
    [ Instance.routes
    , OAuth.routes
    , Accounts.routes
    , Media.routes limits
    , Statuses.routes
    ]

application :: Env -> Wai.Application
application env waiRequest respond = do
  response <- guarded (serve env waiRequest)
  respond (if crossOrigin then Wai.mapResponseHeaders (corsHeaders <>) response else response)
  where
    -- The API answers a page on any origin: a request carries a token,
    -- never a cookie, so another site's script can do nothing with it
    -- that the token's holder could not. The sign-in pages are not
    -- included.
    crossOrigin = case Wai.pathInfo waiRequest of
      "api" : _ -> True
      ["oauth", "token"] -> True
      ["oauth", "revoke"] -> True
      _ -> False
    corsHeaders =
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Expose-Headers", "Link")
      ]

{- | Whatever goes wrong inside a handler is answered with a 500 that says
nothing about the fault, and the fault is written to the log.
-}
guarded :: IO Wai.Response -> IO Wai.Response
guarded action =
  action `catch` \(failure :: SomeException) -> case fromException failure of
    Just (interruption :: SomeAsyncException) -> throwIO interruption
    Nothing -> do
      hPutStrLn stderr ("pellucid-server: request failed: " <> displayException failure)
      pure (stopResponse (refuse status500 "server_error" "the server could not answer this request"))

serve :: Env -> Wai.Request -> IO Wai.Response
serve env waiRequest
  | Wai.requestMethod waiRequest == methodOptions =
      pure
        ( Wai.responseLBS
            status204
            ( [ ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
              , ("Access-Control-Allow-Headers", "Authorization, Content-Type")
              , ("Access-Control-Max-Age", "86400")
              ]
                <> safetyHeaders
            )
            ""
        )
  | otherwise = case dispatch table (Wai.requestMethod waiRequest) path of
      NoRoute -> pure (stopResponse notFound)
      WrongMethod allowed ->
        pure
          ( Wai.mapResponseHeaders
              (("Allow", BS.intercalate ", " allowed) :)
              (stopResponse (refuse status405 "method_not_allowed" "this path does not answer that method"))
          )
      Found found captures -> do
        who <- caller env waiRequest
        case who >>= admit found.access of
          Left stop -> pure (stopResponse stop)
          Right viewer -> do
            inputs <- readParams found.body waiRequest
            case inputs of
              Left why -> pure (stopResponse (refuse status400 "unreadable_request" why))
              Right params -> do
                result <- runExceptT (found.handler env Request {captures, params, viewer, wai = waiRequest})
                pure (either stopResponse id result)
  where
    table = routeTable env.config.limits
    path = filter (not . T.null) (Wai.pathInfo waiRequest)

{- | Who a request's token speaks for. No token is nobody. A token that is
sent and is not good is a refusal on every route, open ones included,
so that a client learns its session has ended from whatever it asks
next.
-}
caller :: Env -> Wai.Request -> IO (Either Stop (Maybe Session))
caller env waiRequest = case bearer waiRequest of
  Nothing -> pure (Right Nothing)
  Just secret -> do
    now <- env.clock
    found <- withConnection env.db \conn -> findSession conn (hashSecret secret)
    pure case found of
      Just row
        | tokenUsable env.config.limits.accessTokenLifetime now row.tokenStatus row.issuedAt && isMember row.accountStatus ->
            Right
              ( Just
                  Session
                    { account = row.account
                    , accountStatus = row.accountStatus
                    , isModerator = row.isModerator
                    , scopes = row.scopes
                    }
              )
      _ -> Left (unauthorized "the token is not valid; sign in again")

bearer :: Wai.Request -> Maybe Text
bearer waiRequest = do
  raw <- lookup hAuthorization (Wai.requestHeaders waiRequest)
  let (scheme, secret) = BS.splitAt 7 raw
  if T.toLower (TE.decodeUtf8With lenientDecode scheme) == "bearer " && not (BS.null secret)
    then Just (T.strip (TE.decodeUtf8With lenientDecode secret))
    else Nothing

-- | Whether a route is open to this caller.
admit :: Access -> Maybe Session -> Either Stop (Maybe Session)
admit access viewer = case (access, viewer) of
  (Open, _) -> Right viewer
  (Signed _, Nothing) -> Left (unauthorized "this route needs a token")
  (Signed need, Just session)
    | satisfies session.scopes need -> Right viewer
    | otherwise -> Left (refuse status403 "insufficient_scope" "the token was not granted the scope this route needs")
