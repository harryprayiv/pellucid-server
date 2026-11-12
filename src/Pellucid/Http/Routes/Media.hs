-- | Uploads, and serving what was uploaded.
module Pellucid.Http.Routes.Media (
  routes,
) where

import Control.Exception (onException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (Value (..))
import Data.ByteString qualified as BS
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (hCacheControl, hContentType, methodDelete, methodGet, methodPost, methodPut, status200, status403, status409, status500)
import Network.Wai qualified as Wai
import Pellucid.Config (Config (..), Env (..))
import Pellucid.Domain.Account (canAct)
import Pellucid.Domain.Ids (MediaId (..), parseId)
import Pellucid.Domain.Limits (Limits (..), maxUploadBytes)
import Pellucid.Domain.OAuth (Need (..))
import Pellucid.Domain.Post (mayTakeBack)
import Pellucid.Domain.Scrub (Scrubbed (..), describeScrubError, scrub)
import Pellucid.Http.Entities (mediaJson)
import Pellucid.Http.Params (BodyLimit (..), FieldProblem (..), Upload (..), boundedText, optionalField, required, upload)
import Pellucid.Http.Respond (Handler, invalid, json, notFound, refuse)
import Pellucid.Http.Router (Access (..), Request (..), Route, Session (..), capture, parse, requireViewer, route)
import Pellucid.Secret (newSecret)
import Pellucid.Store (withConnection)
import Pellucid.Store.Media (MediaRow (..), NewMedia (..), findMedia, findMediaByKey, insertMedia, mediaFile, mediaPath, purgeUnattached, setAltText)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory)

{- | An upload's body may be as large as the largest file plus what a
multipart envelope adds.
-}
routes :: Limits -> [Route]
routes limits =
  [ route methodPost "/api/v2/media" (Signed NeedWrite) uploadBody uploadMedia
  , route methodPost "/api/v1/media" (Signed NeedWrite) uploadBody uploadMedia
  , route methodPut "/api/v1/media/:id" (Signed NeedWrite) SmallBody describeMedia
  , route methodDelete "/api/v1/media/:id" (Signed NeedWrite) SmallBody takeBack
  , route methodGet "/media/:file" Open SmallBody serveMedia
  , route methodGet "/avatar/default.svg" Open SmallBody defaultAvatar
  ]
  where
    uploadBody = UploadBody (maxUploadBytes limits)

{- | Rules MediaReceived, MediaProcessingStarted and MediaProcessed. The
file's type is what its content is, whatever the request called it, and
what is written to disk is the scrubbed file: the bytes as they arrived
are never stored.
-}
uploadMedia :: Env -> Request -> Handler Wai.Response
uploadMedia env request = do
  session <- requireViewer request
  unless (canAct session.accountStatus) $
    throwE (refuse status403 "account_not_active" "this account cannot upload while it is suspended or leaving")
  let limits = env.config.limits
  (file, description) <- parse request ((,) <$> upload "file" <*> optionalField "description" (boundedText limits.maxAltTextLength))
  when (BS.null file.content) $ throwE (invalid [FieldProblem "file" "is empty"])
  when (BS.length file.content > maxUploadBytes limits) $
    throwE (invalid [FieldProblem "file" ("is larger than " <> T.pack (show limits.maxUploadKb) <> " KB")])
  scrubbed <- case scrub file.content of
    Right ok -> pure ok
    Left why -> throwE (invalid [FieldProblem "file" (describeScrubError why)])
  unless (scrubbed.mime `elem` limits.allowedMediaTypes) $
    throwE (invalid [FieldProblem "file" ("is " <> scrubbed.mime <> ", which this server does not accept")])
  key <- liftIO newSecret
  now <- liftIO env.clock
  -- The file is written first, so that a row never names a file that is
  -- not there; if the row cannot be written the file is removed again.
  let path = mediaFile env.config.mediaDir key scrubbed.extension
  stored <- liftIO do
    writeAtomically path scrubbed.bytes
    row <-
      withConnection
        env.db
        ( \conn ->
            insertMedia
              conn
              NewMedia
                { owner = session.account
                , storageKey = key
                , mime = scrubbed.mime
                , extension = scrubbed.extension
                , sizeBytes = fromIntegral (BS.length scrubbed.bytes)
                , dimensions = scrubbed.dimensions
                , altText = maybe "" T.strip description
                , uploadedAt = now
                }
        )
        `onException` removeIfPresent path
    when (isNothing row) (removeIfPresent path)
    pure row
  case stored of
    Nothing -> throwE (refuse status500 "upload_not_stored" "the upload could not be stored")
    Just row -> pure (json status200 [] (mediaJson env.config row))

-- | Write a file so that a reader sees all of it or none of it.
writeAtomically :: FilePath -> BS.ByteString -> IO ()
writeAtomically path bytes = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile (path <> ".part") bytes
  renameFile (path <> ".part") path

describeMedia :: Env -> Request -> Handler Wai.Response
describeMedia env request = do
  session <- requireViewer request
  row <- ownMedia env session request
  description <- parse request (required "description" (boundedText env.config.limits.maxAltTextLength))
  updated <- liftIO . withConnection env.db $ \conn -> do
    setAltText conn row.mediaId session.account (T.strip description)
    findMedia conn row.mediaId
  maybe (throwE notFound) (pure . json status200 [] . mediaJson env.config) updated

-- | Rule MediaTakenBack (S13).
takeBack :: Env -> Request -> Handler Wai.Response
takeBack env request = do
  session <- requireViewer request
  row <- ownMedia env session request
  unless (mayTakeBack True row.status) $
    throwE (refuse status409 "media_in_use" "this upload is on a post; it leaves with the post or by an edit that names it")
  purged <- liftIO (withConnection env.db \conn -> purgeUnattached conn row.mediaId session.account)
  unless purged $
    throwE (refuse status409 "media_in_use" "this upload is on a post; it leaves with the post or by an edit that names it")
  liftIO (removeIfPresent (mediaPath env.config.mediaDir row))
  pure (json status200 [] (Object mempty))

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  present <- doesFileExist path
  when present (removeFile path)

{- | The caller's own upload named by the path. Someone else's is the same
missing page as one that does not exist.
-}
ownMedia :: Env -> Session -> Request -> Handler MediaRow
ownMedia env session request = do
  raw <- capture 0 request
  found <- case parseId raw of
    Just n -> liftIO (withConnection env.db \conn -> findMedia conn (MediaId n))
    Nothing -> pure Nothing
  case found of
    Just row | row.owner == session.account -> pure row
    _ -> throwE notFound

{- | A stored file by its key. The key is 256 random bits, and knowing it
is what lets a reader fetch the file: there is no further check, which
is how an image tag in a client loads a picture from a followers-only
post without sending a token.

The answer is typed as what the scrubber found the file to be, is not
to be reinterpreted, and may not run anything if it is opened directly.
-}
serveMedia :: Env -> Request -> Handler Wai.Response
serveMedia env request = do
  name <- capture 0 request
  let key = T.takeWhile (/= '.') name
  found <- liftIO (withConnection env.db \conn -> findMediaByKey conn key)
  row <- maybe (throwE notFound) pure found
  when (name /= row.storageKey <> "." <> row.extension) (throwE notFound)
  let path = mediaPath env.config.mediaDir row
  present <- liftIO (doesFileExist path)
  unless present (throwE notFound)
  pure
    ( Wai.responseFile
        status200
        [ (hContentType, TE.encodeUtf8 row.mime)
        , (hCacheControl, "public, max-age=31536000, immutable")
        , ("X-Content-Type-Options", "nosniff")
        , ("Content-Security-Policy", "default-src 'none'; sandbox")
        , ("Cross-Origin-Resource-Policy", "cross-origin")
        ]
        path
        Nothing
    )

-- | The picture an account has until it uploads one, which is not built.
defaultAvatar :: Env -> Request -> Handler Wai.Response
defaultAvatar _ _ =
  pure
    ( Wai.responseLBS
        status200
        [ (hContentType, "image/svg+xml")
        , (hCacheControl, "public, max-age=86400")
        , ("X-Content-Type-Options", "nosniff")
        , ("Content-Security-Policy", "default-src 'none'; sandbox")
        ]
        "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 64 64\"><rect width=\"64\" height=\"64\" fill=\"#d8d8d8\"/><circle cx=\"32\" cy=\"25\" r=\"11\" fill=\"#8a8a8a\"/><path d=\"M10 58c2-13 12-19 22-19s20 6 22 19z\" fill=\"#8a8a8a\"/></svg>"
    )
