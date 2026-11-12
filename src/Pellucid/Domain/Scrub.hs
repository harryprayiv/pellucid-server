{- | What happens to an upload before a byte of it is stored (rule
MediaProcessingStarted): its type is decided by its content, and
everything in the file that is not needed to draw the picture is left
out. That removes the EXIF block with its position, time, device model
and serial number, the embedded thumbnail (which can show the picture as
it was before a crop), XMP and IPTC records, comments, and anything
appended after the end of the image.

The file is never decoded. It is walked segment by segment (JPEG) or
chunk by chunk (PNG) and each piece is kept or dropped by its type, so
the picture data is the uploader's own, bit for bit, and no image
decoder runs on bytes a stranger sent. Kept: the pieces a decoder needs,
the colour profile, and for a JPEG the orientation, rewritten as an EXIF
block that holds that one value and nothing else.

A type this module cannot scrub is refused. GIF, WebP and MP4 are in the
spec's list of accepted types and are not accepted here yet.
-}
module Pellucid.Domain.Scrub (
  Scrubbed (..),
  ScrubError (..),
  scrub,
  describeScrubError,
  maxSide,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard, mfilter)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)

data Scrubbed = Scrubbed
  { bytes :: ByteString
  , mime :: Text
  , extension :: Text
  , dimensions :: Maybe (Int, Int)
  -- ^ Width and height as the picture is displayed, which for a JPEG with
  -- a quarter-turn orientation is the stored size turned.
  }
  deriving stock (Eq, Show)

data ScrubError
  = UnknownFormat
  | Malformed Text
  deriving stock (Eq, Show)

describeScrubError :: ScrubError -> Text
describeScrubError = \case
  UnknownFormat -> "the file is not a JPEG or a PNG"
  Malformed why -> "the file is not a complete image: " <> why

{- | The longest side accepted, in pixels. The file is not decoded here,
but every client that shows it will decode it.
-}
maxSide :: Int
maxSide = 16384

scrub :: ByteString -> Either ScrubError Scrubbed
scrub input
  | BS.pack [0xFF, 0xD8, 0xFF] `BS.isPrefixOf` input = scrubJpeg input
  | pngSignature `BS.isPrefixOf` input = scrubPng input
  | otherwise = Left UnknownFormat

-- JPEG ------------------------------------------------------------------------

data Jpeg = Jpeg
  { jfif :: Maybe ByteString
  , kept :: [ByteString]
  -- ^ Newest first.
  , orientation :: Maybe Int
  , frame :: Maybe (Int, Int)
  , scans :: Int
  }

scrubJpeg :: ByteString -> Either ScrubError Scrubbed
scrubJpeg input = do
  jpeg <- segments (BS.drop 2 input) (Jpeg Nothing [] Nothing Nothing 0)
  (width, height) <- maybe (Left (Malformed "it has no image frame")) Right jpeg.frame
  guardThat (jpeg.scans > 0) "it has no image data"
  guardThat (width <= maxSide && height <= maxSide) ("a side is longer than " <> T.pack (show maxSide) <> " pixels")
  let
    turn = mfilter (> 1) jpeg.orientation
    quarterTurned = maybe False (>= 5) turn
  pure
    Scrubbed
      { bytes =
          BS.concat
            ( [BS.pack [0xFF, 0xD8]]
                <> maybe [] pure jpeg.jfif
                <> maybe [] (pure . orientationSegment) turn
                <> reverse jpeg.kept
                <> [BS.pack [0xFF, 0xD9]]
            )
      , mime = "image/jpeg"
      , extension = "jpg"
      , dimensions = Just (if quarterTurned then (height, width) else (width, height))
      }

{- | The segments before the first scan and between scans, up to the end of
image marker. What follows that marker is not looked at and not kept.
-}
segments :: ByteString -> Jpeg -> Either ScrubError Jpeg
segments input jpeg = do
  (marker, rest) <- nextMarker input
  case marker of
    0xD9 -> Right jpeg
    0xD8 -> Left (Malformed "it starts a second image inside the first")
    _
      | standalone marker -> segments rest jpeg {kept = BS.pack [0xFF, marker] : jpeg.kept}
      | otherwise -> do
          (payload, after) <- lengthPrefixed rest
          let next = classify marker payload jpeg
          if marker == 0xDA then scan after next {scans = next.scans + 1} else segments after next

nextMarker :: ByteString -> Either ScrubError (Word8, ByteString)
nextMarker input = case BS.uncons input of
  Just (0xFF, rest) -> case BS.uncons (BS.dropWhile (== 0xFF) rest) of
    Just (marker, after) | marker /= 0x00 -> Right (marker, after)
    _ -> Left truncated
  Just _ -> Left (Malformed "it has bytes between segments that belong to none")
  Nothing -> Left truncated

-- | Markers that stand alone, with no length and no payload.
standalone :: Word8 -> Bool
standalone marker = marker == 0x01 || (marker >= 0xD0 && marker <= 0xD7)

lengthPrefixed :: ByteString -> Either ScrubError (ByteString, ByteString)
lengthPrefixed input = case be16 input 0 of
  Just size | size >= 2 && BS.length input >= size -> Right (BS.take (size - 2) (BS.drop 2 input), BS.drop size input)
  _ -> Left truncated

segmentBytes :: Word8 -> ByteString -> ByteString
segmentBytes marker payload =
  let size = BS.length payload + 2
   in BS.pack [0xFF, marker, fromIntegral (size `div` 256), fromIntegral (size `mod` 256)] <> payload

{- | Keep or drop one segment.

  APP0   the JFIF header is kept, cut down to its fixed part so that the
         thumbnail it may carry is gone; a JFXX extension is a thumbnail
         and is dropped
  APP1   EXIF and XMP: dropped, after the orientation is read from EXIF
  APP2   kept when it is a colour profile, dropped otherwise
  APP14  the Adobe marker says how the colour components are encoded and
         is kept
  other application segments and comments: dropped
  everything else is what a decoder needs and is kept
-}
classify :: Word8 -> ByteString -> Jpeg -> Jpeg
classify marker payload jpeg
  | marker == 0xE0 =
      if "JFIF\0" `BS.isPrefixOf` payload && BS.length payload >= 14 && isNothing jpeg.jfif
        then jpeg {jfif = Just (segmentBytes 0xE0 (BS.take 12 payload <> BS.pack [0, 0]))}
        else jpeg
  | marker == 0xE1 =
      if "Exif\0\0" `BS.isPrefixOf` payload && isNothing jpeg.orientation
        then jpeg {orientation = tiffOrientation (BS.drop 6 payload)}
        else jpeg
  | marker == 0xE2 = if "ICC_PROFILE\0" `BS.isPrefixOf` payload then keep else jpeg
  | marker == 0xEE = if "Adobe" `BS.isPrefixOf` payload then keep else jpeg
  | marker >= 0xE3 && marker <= 0xEF = jpeg
  | marker == 0xFE = jpeg
  | isFrame = keep {frame = jpeg.frame <|> frameSize}
  | otherwise = keep
  where
    keep = jpeg {kept = segmentBytes marker payload : jpeg.kept}
    isFrame = marker >= 0xC0 && marker <= 0xCF && marker `notElem` [0xC4, 0xC8, 0xCC]
    frameSize = do
      height <- be16 payload 1
      width <- be16 payload 3
      guard (width > 0 && height > 0)
      pure (width, height)

{- | The entropy-coded data of one scan, kept whole. Inside it a 0xFF is
followed by 0x00 (a literal 0xFF) or by a restart marker; anything else
after a 0xFF is the marker that ends the scan.
-}
scan :: ByteString -> Jpeg -> Either ScrubError Jpeg
scan input jpeg = do
  end <- endOfScan input 0
  segments (BS.drop end input) jpeg {kept = BS.take end input : jpeg.kept}

endOfScan :: ByteString -> Int -> Either ScrubError Int
endOfScan input from = case BS.elemIndex 0xFF (BS.drop from input) of
  Nothing -> Left truncated
  Just offset ->
    let at = from + offset
     in case BS.indexMaybe input (at + 1) of
          Nothing -> Left truncated
          Just next
            | next == 0x00 || (next >= 0xD0 && next <= 0xD7) -> endOfScan input (at + 2)
            | next == 0xFF -> endOfScan input (at + 1)
            | otherwise -> Right at

{- | The orientation tag (0x0112, a 16-bit value from 1 to 8) of the first
image directory in a TIFF structure, which is what an EXIF block is.
Every read is bounds-checked; a structure that does not parse has no
orientation.
-}
tiffOrientation :: ByteString -> Maybe Int
tiffOrientation tiff = do
  bigEndian <- case BS.take 2 tiff of
    "MM" -> Just True
    "II" -> Just False
    _ -> Nothing
  let
    u16 at = if bigEndian then be16 tiff at else le16 tiff at
    u32 at = do
      a <- u16 at
      b <- u16 (at + 2)
      pure (if bigEndian then a * 65536 + b else b * 65536 + a)
  magic <- u16 2
  guard (magic == 42)
  directory <- u32 4
  count <- u16 directory
  listToMaybe
    [ value
    | index <- [0 .. min count 256 - 1]
    , let entry = directory + 2 + 12 * index
    , Just 0x0112 <- [u16 entry]
    , Just 3 <- [u16 (entry + 2)]
    , Just value <- [u16 (entry + 8)]
    , value >= 1 && value <= 8
    ]

-- | An EXIF segment that says one thing: how to turn the picture.
orientationSegment :: Int -> ByteString
orientationSegment value =
  segmentBytes 0xE1 $
    "Exif\0\0"
      <> BS.pack
        [ 0x4D
        , 0x4D
        , 0x00
        , 0x2A -- big-endian TIFF
        , 0
        , 0
        , 0
        , 8 -- the directory follows the header
        , 0
        , 1 -- one entry
        , 0x01
        , 0x12 -- orientation
        , 0
        , 3 -- a 16-bit value
        , 0
        , 0
        , 0
        , 1 -- one of them
        , 0
        , fromIntegral value
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0 -- no further directory
        ]

-- PNG -------------------------------------------------------------------------

pngSignature :: ByteString
pngSignature = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

{- | The chunks a decoder draws from. Text chunks, the EXIF chunk, the
modification time and every chunk not named here are dropped.
-}
pngKept :: [ByteString]
pngKept =
  [ "IHDR"
  , "PLTE"
  , "IDAT"
  , "IEND"
  , "tRNS"
  , "gAMA"
  , "cHRM"
  , "sRGB"
  , "iCCP"
  , "sBIT"
  , "bKGD"
  , "pHYs"
  , "hIST"
  , "cICP"
  , "acTL"
  , "fcTL"
  , "fdAT"
  ]

scrubPng :: ByteString -> Either ScrubError Scrubbed
scrubPng input = do
  (header, afterHeader) <- pngChunk (BS.drop 8 input)
  guardThat (BS.take 4 (BS.drop 4 header) == "IHDR" && BS.length header == 25) "its first chunk is not the image header"
  width <- maybe (Left truncated) Right (be32 header 8)
  height <- maybe (Left truncated) Right (be32 header 12)
  guardThat (width > 0 && height > 0) "it has no size"
  guardThat (width <= maxSide && height <= maxSide) ("a side is longer than " <> T.pack (show maxSide) <> " pixels")
  rest <- pngChunks afterHeader []
  guardThat (any (\chunk -> BS.take 4 (BS.drop 4 chunk) == "IDAT") rest) "it has no image data"
  pure
    Scrubbed
      { bytes = BS.concat (pngSignature : header : rest)
      , mime = "image/png"
      , extension = "png"
      , dimensions = Just (width, height)
      }

{- | Every kept chunk up to and including the end chunk. What follows the
end chunk is not kept.
-}
pngChunks :: ByteString -> [ByteString] -> Either ScrubError [ByteString]
pngChunks input acc = do
  (chunk, rest) <- pngChunk input
  let kind = BS.take 4 (BS.drop 4 chunk)
  if kind == "IEND"
    then Right (reverse (chunk : acc))
    else pngChunks rest (if kind `elem` pngKept then chunk : acc else acc)

-- | One chunk whole: length, type, data and checksum.
pngChunk :: ByteString -> Either ScrubError (ByteString, ByteString)
pngChunk input = case be32 input 0 of
  Just size | BS.length input >= size + 12 -> Right (BS.splitAt (size + 12) input)
  _ -> Left truncated

-- Reading numbers ---------------------------------------------------------------

be16 :: ByteString -> Int -> Maybe Int
be16 input at = do
  guard (at >= 0)
  high <- BS.indexMaybe input at
  low <- BS.indexMaybe input (at + 1)
  pure (fromIntegral high * 256 + fromIntegral low)

le16 :: ByteString -> Int -> Maybe Int
le16 input at = do
  guard (at >= 0)
  low <- BS.indexMaybe input at
  high <- BS.indexMaybe input (at + 1)
  pure (fromIntegral high * 256 + fromIntegral low)

be32 :: ByteString -> Int -> Maybe Int
be32 input at = do
  high <- be16 input at
  low <- be16 input (at + 2)
  pure (high * 65536 + low)

truncated :: ScrubError
truncated = Malformed "it ends before the image does"

guardThat :: Bool -> Text -> Either ScrubError ()
guardThat ok why = if ok then Right () else Left (Malformed why)
