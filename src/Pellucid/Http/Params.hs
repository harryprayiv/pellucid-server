{- | A request's inputs, read the way the API this server is compatible
with reads them: the query string, a form body, a multipart body and a
JSON body are one set of named values, and a route does not care which
of them a client used. Pixelfed's own clients send all four.

A route describes what it needs with a 'Parser'. Parsers combine
applicatively, so a request with three bad fields is answered with all
three, each named (contract ClientApiCompat, invariant ErrorsSayWhy).
-}
module Pellucid.Http.Params (
  Params,
  Upload (..),
  BodyLimit (..),
  readParams,
  paramsFromList,
  FieldProblem (..),
  Parser,
  runParser,
  Reader,
  required,
  optionalField,
  withDefault,
  listOf,
  upload,
  text,
  int,
  bool,
  boundedText,
) where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Read qualified as TR
import Network.HTTP.Types (hContentType)
import Network.Wai qualified as Wai
import Network.Wai.Parse qualified as Parse

data Upload = Upload
  { fileName :: Text
  , declaredType :: Text
  , content :: ByteString
  }

data Params = Params
  { values :: Map Text [Text]
  , uploads :: Map Text Upload
  }

{- | How large a body a route reads. Only the upload routes read a large
one.
-}
data BodyLimit
  = SmallBody
  | UploadBody Int

smallBodyBytes :: Int
smallBodyBytes = 1024 * 1024

paramsFromList :: [(Text, Text)] -> Params
paramsFromList pairs = Params {values = Map.fromListWith (flip (<>)) [(normalise k, [v]) | (k, v) <- pairs], uploads = Map.empty}

{- | @media_ids[]@ and @media_ids@ are the same name: the brackets are how
a form says a name repeats.
-}
normalise :: Text -> Text
normalise key = fromMaybe key (T.stripSuffix "[]" key)

{- | Read everything a request carries. 'Left' when the body cannot be
read as what its content type says it is, or is larger than the route
allows.
-}
readParams :: BodyLimit -> Wai.Request -> IO (Either Text Params)
readParams limit request = do
  let
    fromQuery = [(decode k, maybe "" decode v) | (k, v) <- Wai.queryString request]
    contentType = fromMaybe "" (lookup hContentType (Wai.requestHeaders request))
    allowed = case limit of
      SmallBody -> smallBodyBytes
      UploadBody bytes -> bytes
  body <-
    if "application/json" `BS.isPrefixOf` contentType
      then readJson allowed request
      else
        if "application/x-www-form-urlencoded" `BS.isPrefixOf` contentType || "multipart/form-data" `BS.isPrefixOf` contentType
          then readForm allowed request
          else pure (Right ([], []))
  pure do
    (fromBody, files) <- body
    Right
      Params
        { values = Map.fromListWith (flip (<>)) [(normalise k, [v]) | (k, v) <- fromQuery <> fromBody]
        , uploads = Map.fromList [(normalise k, file) | (k, file) <- files]
        }

decode :: ByteString -> Text
decode = TE.decodeUtf8With lenientDecode

readJson :: Int -> Wai.Request -> IO (Either Text ([(Text, Text)], [(Text, Upload)]))
readJson allowed request = do
  body <- readLimited allowed request
  pure case body of
    Nothing -> Left "the request body is larger than this route accepts"
    Just bytes
      | BS.null bytes -> Right ([], [])
      | otherwise -> case Aeson.decodeStrict bytes of
          Just (Aeson.Object object) -> Right (concatMap flatten (KeyMap.toList object), [])
          Just _ -> Left "a JSON body must be an object"
          Nothing -> Left "the request body is not valid JSON"
  where
    flatten (key, value) = [(Key.toText key, rendered) | rendered <- scalars value]
    scalars = \case
      Aeson.String s -> [s]
      Aeson.Bool b -> [if b then "true" else "false"]
      n@(Aeson.Number _) -> [decode (BL.toStrict (Aeson.encode n))]
      Aeson.Array items -> concatMap scalars (toList items)
      Aeson.Null -> []
      Aeson.Object _ -> []

{- | The body, unless it is longer than allowed. It is read in chunks and
reading stops at the limit, so a large body is not held in memory.
-}
readLimited :: Int -> Wai.Request -> IO (Maybe ByteString)
readLimited allowed request = go 0 []
  where
    go size chunks = do
      chunk <- Wai.getRequestBodyChunk request
      if BS.null chunk
        then pure (Just (BS.concat (reverse chunks)))
        else
          let size' = size + BS.length chunk
           in if size' > allowed then pure Nothing else go size' (chunk : chunks)

readForm :: Int -> Wai.Request -> IO (Either Text ([(Text, Text)], [(Text, Upload)]))
readForm allowed request = do
  let options =
        Parse.setMaxRequestFileSize (fromIntegral allowed) $
          Parse.setMaxRequestNumFiles 1 $
            Parse.setMaxRequestParmsSize smallBodyBytes Parse.defaultParseRequestBodyOptions
  result <- try (Parse.parseRequestBodyEx options Parse.lbsBackEnd request)
  pure case result of
    Left (_ :: SomeException) -> Left "the request body could not be read, or is larger than this route accepts"
    Right (fields, files) ->
      Right
        ( [(decode k, decode v) | (k, v) <- fields]
        , [ ( decode k
            , Upload
                { fileName = decode (Parse.fileName file)
                , declaredType = decode (Parse.fileContentType file)
                , content = BL.toStrict (Parse.fileContent file)
                }
            )
          | (k, file) <- files
          ]
        )

-- Parsers ---------------------------------------------------------------------

data FieldProblem = FieldProblem
  { field :: Text
  , problem :: Text
  }
  deriving stock (Eq, Show)

newtype Parser a = Parser (Params -> Either [FieldProblem] a)

instance Functor Parser where
  fmap f (Parser run) = Parser (fmap f . run)

{- | Both sides are run and their problems joined, which is what makes a
refusal list every field at once. There is no 'Monad' instance on
purpose: with one, the second field could not be checked until the
first had passed.
-}
instance Applicative Parser where
  pure a = Parser (const (Right a))
  Parser runF <*> Parser runA = Parser \params -> case (runF params, runA params) of
    (Right f, Right a) -> Right (f a)
    (Left problems, Right _) -> Left problems
    (Right _, Left problems) -> Left problems
    (Left first, Left second) -> Left (first <> second)

runParser :: Parser a -> Params -> Either [FieldProblem] a
runParser (Parser run) = run

{- | Turns one value into what the route wants, or says what is wrong with
it.
-}
type Reader a = Text -> Either Text a

firstValue :: Text -> Params -> Maybe Text
firstValue name params = case Map.lookup name params.values of
  Just (value : _) -> Just value
  _ -> Nothing

required :: Text -> Reader a -> Parser a
required name reader = Parser \params -> case firstValue name params of
  Nothing -> Left [FieldProblem name "is required"]
  Just value -> either (\why -> Left [FieldProblem name why]) Right (reader value)

-- | Absent, or present and empty, is 'Nothing'.
optionalField :: Text -> Reader a -> Parser (Maybe a)
optionalField name reader = Parser \params -> case firstValue name params of
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just value -> either (\why -> Left [FieldProblem name why]) (Right . Just) (reader value)

withDefault :: a -> Text -> Reader a -> Parser a
withDefault fallback name reader = fromMaybe fallback <$> optionalField name reader

listOf :: Text -> Reader a -> Parser [a]
listOf name reader = Parser \params ->
  either (\why -> Left [FieldProblem name why]) Right (traverse reader (fromMaybe [] (Map.lookup name params.values)))

upload :: Text -> Parser Upload
upload name = Parser \params -> case Map.lookup name params.uploads of
  Just file -> Right file
  Nothing -> Left [FieldProblem name "is required, as a file in a multipart body"]

text :: Reader Text
text = Right

-- | Text of at most this many characters.
boundedText :: Int -> Reader Text
boundedText limit value
  | T.length value > limit = Left ("is longer than " <> T.pack (show limit) <> " characters")
  | otherwise = Right value

int :: Reader Int
int value = case TR.signed TR.decimal value of
  Right (n, "") | abs n < (1000000000 :: Integer) -> Right (fromInteger n)
  _ -> Left "is not a whole number"

-- | The spellings clients of this API use for a flag.
bool :: Reader Bool
bool value = case T.toLower value of
  v | v `elem` ["true", "1", "on", "yes"] -> Right True
  v | v `elem` ["false", "0", "off", "no", ""] -> Right False
  _ -> Left "is neither true nor false"
