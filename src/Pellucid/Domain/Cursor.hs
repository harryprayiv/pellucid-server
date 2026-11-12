{- | S12, one way to page. A list is read newest first; a cursor names the
last item a page showed and the next page holds what lies below it. To a
client the cursor is an opaque string in a Link header.
-}
module Pellucid.Domain.Cursor (
  Cursor (..),
  encodeCursor,
  decodeCursor,
  Page (..),
  paginate,
  clampLimit,
) where

import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Pellucid.Domain.Ids (idText, parseId)

-- | Everything with an id below this one.
newtype Cursor = Cursor Int64
  deriving stock (Eq, Show)

{- | The leading @c1@ names the encoding, so that a later one can be told
apart from this one and old cursors refused instead of misread.
-}
encodeCursor :: Cursor -> Text
encodeCursor (Cursor n) = TE.decodeUtf8 (Base64Url.encodeUnpadded (TE.encodeUtf8 ("c1:" <> idText n)))

decodeCursor :: Text -> Maybe Cursor
decodeCursor raw = do
  bytes <- either (const Nothing) Just (Base64Url.decodeUnpadded (TE.encodeUtf8 raw))
  text <- either (const Nothing) Just (TE.decodeUtf8' bytes)
  Cursor <$> (T.stripPrefix "c1:" text >>= parseId)

data Page a = Page
  { items :: [a]
  , next :: Maybe Cursor
  }
  deriving stock (Eq, Show)

{- | Cut a page out of rows that were fetched one past the limit. The extra
row is how the page knows that more exists, so a list that ends exactly
at a page boundary carries no cursor and costs no empty request.
-}
paginate :: Int -> (a -> Int64) -> [a] -> Page a
paginate limit idOf rows =
  let
    shown = take limit rows
    more = length rows > limit
   in
    Page
      { items = shown
      , next = case reverse shown of
          lastShown : _ | more -> Just (Cursor (idOf lastShown))
          _ -> Nothing
      }

{- | The page size a request gets: what it asked for, kept between one and
the largest page, or the default when it asked for nothing.
-}
clampLimit :: Int -> Int -> Maybe Int -> Int
clampLimit standard largest = maybe standard (max 1 . min largest)
