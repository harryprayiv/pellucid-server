{-# LANGUAGE QuasiQuotes #-}

{- | Uploads: their rows here, their bytes in the media directory under a
random key.
-}
module Pellucid.Store.Media (
  MediaRow (..),
  NewMedia (..),
  insertMedia,
  findMedia,
  findMediaByKey,
  mediaOfPosts,
  setAltText,
  purgeUnattached,
  mediaPath,
  mediaFile,
) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), Query, execute, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pellucid.Domain.Ids (AccountId (..), MediaId (..), PostId (..))
import Pellucid.Domain.Post (MediaStatus, parseMediaStatus)
import Pellucid.Store (parsed)
import System.FilePath ((<.>), (</>))

data MediaRow = MediaRow
  { mediaId :: MediaId
  , owner :: AccountId
  , post :: Maybe PostId
  , storageKey :: Text
  , mime :: Text
  , extension :: Text
  , sizeBytes :: Int64
  , width :: Maybe Int
  , height :: Maybe Int
  , altText :: Text
  , status :: MediaStatus
  }

instance FromRow MediaRow where
  fromRow =
    MediaRow
      <$> (MediaId <$> field)
      <*> (AccountId <$> field)
      <*> (fmap PostId <$> field)
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> parsed parseMediaStatus

selectMedia :: Query
selectMedia = "SELECT id, owner_id, post_id, storage_key, mime, extension, size_bytes, width, height, alt_text, status FROM media"

data NewMedia = NewMedia
  { owner :: AccountId
  , storageKey :: Text
  , mime :: Text
  , extension :: Text
  , sizeBytes :: Int64
  , dimensions :: Maybe (Int, Int)
  , altText :: Text
  , uploadedAt :: UTCTime
  }

{- | Rules MediaReceived, MediaProcessingStarted and MediaProcessed in one
step: the bytes were scrubbed before this is called, so the row is
written once, as ready. A file that cannot be scrubbed gets no row.
-}
insertMedia :: Connection -> NewMedia -> IO (Maybe MediaRow)
insertMedia conn new = do
  let AccountId owner = new.owner
  rows <-
    query
      conn
      [sql|
        INSERT INTO media (owner_id, storage_key, mime, extension, size_bytes, width, height, alt_text, uploaded_at, status)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'ready')
        RETURNING id
      |]
      (owner, new.storageKey, new.mime, new.extension, new.sizeBytes, fst <$> new.dimensions, snd <$> new.dimensions, new.altText, new.uploadedAt)
  case rows of
    Only n : _ -> findMedia conn (MediaId n)
    [] -> pure Nothing

findMedia :: Connection -> MediaId -> IO (Maybe MediaRow)
findMedia conn (MediaId n) = listToMaybe <$> query conn (selectMedia <> " WHERE id = ?") (Only n)

-- | For serving. A purged row's file is gone and its key finds nothing.
findMediaByKey :: Connection -> Text -> IO (Maybe MediaRow)
findMediaByKey conn key = listToMaybe <$> query conn (selectMedia <> " WHERE storage_key = ? AND status <> 'purged'") (Only key)

-- | The attachments of the given posts, each post's in album order.
mediaOfPosts :: Connection -> [PostId] -> IO [MediaRow]
mediaOfPosts _ [] = pure []
mediaOfPosts conn posts =
  query conn (selectMedia <> " WHERE status = 'attached' AND post_id IN ? ORDER BY post_id, position") (Only (In [n | PostId n <- posts]))

setAltText :: Connection -> MediaId -> AccountId -> Text -> IO ()
setAltText conn (MediaId n) (AccountId owner) text =
  void (execute conn "UPDATE media SET alt_text = ? WHERE id = ? AND owner_id = ? AND status <> 'purged'" (text, n, owner))

{- | Rule MediaTakenBack's effect (S13): the owner's upload, while it is
not on a post. 'True' when a row was purged; the caller then removes the
file.
-}
purgeUnattached :: Connection -> MediaId -> AccountId -> IO Bool
purgeUnattached conn (MediaId n) (AccountId owner) =
  (> 0) <$> execute conn "UPDATE media SET status = 'purged' WHERE id = ? AND owner_id = ? AND status IN ('ready', 'unusable')" (n, owner)

{- | Where a media row's bytes live under the media directory. The first
two characters of the key make a subdirectory so that no directory
grows without bound.
-}
mediaPath :: FilePath -> MediaRow -> FilePath
mediaPath root row = mediaFile root row.storageKey row.extension

{- | The same path from a key and an extension, for a file that is written
before its row is.
-}
mediaFile :: FilePath -> Text -> Text -> FilePath
mediaFile root key extension = root </> T.unpack (T.take 2 key) </> T.unpack key <.> T.unpack extension
