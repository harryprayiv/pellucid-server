{- | Answers: JSON, refusals that say why, and the Link header a list pages
by.
-}
module Pellucid.Http.Respond (
  Handler,
  Stop (..),
  ApiError (..),
  ErrorShape (..),
  stopResponse,
  refuse,
  invalid,
  notFound,
  unauthorized,
  forbidden,
  oauthError,
  json,
  errorResponse,
  timestamp,
  escapeHtml,
  pageLink,
  safetyHeaders,
) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (Header, ResponseHeaders, Status, hCacheControl, hContentType, status400, status401, status403, status404, status422, urlEncode)
import Network.Wai qualified as Wai
import Pellucid.Domain.Cursor (Cursor, encodeCursor)
import Pellucid.Http.Params (FieldProblem (..))

-- | A handler runs until it has an answer or until it stops early.
type Handler = ExceptT Stop IO

{- | Why a handler stopped early: with a refusal, which is sent as JSON,
or with an answer of its own, which is how the sign-in pages leave.
-}
data Stop
  = Stop ApiError
  | Answer Wai.Response

stopResponse :: Stop -> Wai.Response
stopResponse = \case
  Stop err -> errorResponse err
  Answer response -> response

{- | The client API answers a refusal with @error@ as a sentence; the OAuth
endpoints answer with @error@ as one of RFC 6749's codes and the
sentence beside it. Both carry @error_code@.
-}
data ErrorShape = ApiShape | OAuthShape

data ApiError = ApiError
  { status :: Status
  , code :: Text
  , message :: Text
  , fields :: [FieldProblem]
  , shape :: ErrorShape
  }

refuse :: Status -> Text -> Text -> Stop
refuse status code message = Stop ApiError {status, code, message, fields = [], shape = ApiShape}

-- | A request whose fields are wrong, with each one named.
invalid :: [FieldProblem] -> Stop
invalid fields =
  Stop
    ApiError
      { status = status422
      , code = "validation_failed"
      , message = T.intercalate "; " [problem.field <> " " <> problem.problem | problem <- fields]
      , fields
      , shape = ApiShape
      }

notFound :: Stop
notFound = refuse status404 "not_found" "there is nothing here that you can see"

unauthorized :: Text -> Stop
unauthorized = refuse status401 "invalid_token"

forbidden :: Text -> Text -> Stop
forbidden = refuse status403

oauthError :: Text -> Text -> Stop
oauthError code message =
  Stop
    ApiError
      { status = if code == "invalid_client" then status401 else status400
      , code
      , message
      , fields = []
      , shape = OAuthShape
      }

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Wai.Response
json status headers value =
  Wai.responseLBS status ((hContentType, "application/json; charset=utf-8") : safetyHeaders <> headers) (Aeson.encode value)

{- | Sent with every answer. Nothing this server says should be cached by
something in between or reinterpreted as another type.
-}
safetyHeaders :: ResponseHeaders
safetyHeaders =
  [ (hCacheControl, "no-store")
  , ("X-Content-Type-Options", "nosniff")
  , ("Referrer-Policy", "no-referrer")
  ]

errorResponse :: ApiError -> Wai.Response
errorResponse err = json err.status [("WWW-Authenticate", "Bearer") | err.status == status401] body
  where
    body = object (lead <> ["error_code" .= err.code] <> ["fields" .= map fieldJson err.fields | not (null err.fields)])
    lead = case err.shape of
      ApiShape -> ["error" .= err.message]
      OAuthShape -> ["error" .= err.code, "error_description" .= err.message]
    fieldJson problem = object ["field" .= problem.field, "problem" .= problem.problem]

{- | ISO 8601 in UTC with three fractional digits, the form the API this
server is compatible with uses for a post.
-}
timestamp :: UTCTime -> Text
timestamp time =
  let
    whole = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time)
    millis = (floor (utcTimeToPOSIXSeconds time * 1000) :: Integer) `mod` 1000
    padded = T.justifyRight 3 '0' (T.pack (show millis))
   in
    whole <> "." <> padded <> "Z"

-- | Text for use inside HTML, in an element or in a quoted attribute.
escapeHtml :: Text -> Text
escapeHtml = T.concatMap \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  c -> T.singleton c

{- | S12: the header that leads further into a list. @base@ is the
absolute URL of the list with whatever query the next request must
repeat (the page size); the cursor is added to it.
-}
pageLink :: Text -> Maybe Cursor -> [Header]
pageLink base = \case
  Nothing -> []
  Just cursor ->
    let
      joiner = if T.any (== '?') base then "&" else "?"
      url = base <> joiner <> "cursor=" <> TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 (encodeCursor cursor)))
     in
      [("Link", TE.encodeUtf8 ("<" <> url <> ">; rel=\"next\""))]
