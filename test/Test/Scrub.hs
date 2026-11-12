{- | The scrubber against files built here segment by segment, so that
each test says exactly what went in. The pictures are not decodable
(their image data is a few arbitrary bytes), which the scrubber cannot
tell: it never decodes.
-}
module Test.Scrub (
  tests,
  sampleJpeg,
  secrets,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Word (Word8)
import Pellucid.Domain.Scrub (ScrubError (..), Scrubbed (..), maxSide, scrub)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (elements, forAll, listOf, testProperty, vectorOf, (===))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "scrub"
    [ testCase "a JPEG loses everything that is not the picture" do
        out <- scrubbed sampleJpeg
        mapM_ (\secret -> assertBool (show secret <> " is gone") (not (secret `BS.isInfixOf` out.bytes))) secrets
        out.mime @?= "image/jpeg"
        out.extension @?= "jpg"
        assertBool "starts with the JFIF header" (BS.pack [0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10] `BS.isPrefixOf` out.bytes)
        assertBool "ends at the end of image" (BS.pack [0xFF, 0xD9] `BS.isSuffixOf` out.bytes)
        assertBool "keeps the colour profile" ("ICC_PROFILE" `BS.isInfixOf` out.bytes)
        assertBool "keeps the image data, stuffed bytes and restart marker included" (entropy `BS.isInfixOf` out.bytes)
    , testCase "the orientation is kept, alone, and the size is the size as displayed" do
        out <- scrubbed sampleJpeg
        assertBool
          "an EXIF block holding one entry"
          (("Exif\0\0" <> BS.pack [0x4D, 0x4D, 0, 0x2A, 0, 0, 0, 8, 0, 1, 0x01, 0x12, 0, 3, 0, 0, 0, 1, 0, 6]) `BS.isInfixOf` out.bytes)
        out.dimensions @?= Just (16, 32)
    , testCase "a JPEG with no orientation gets no EXIF block" do
        out <- scrubbed (jpeg [jfif] [])
        assertBool "no EXIF" (not ("Exif" `BS.isInfixOf` out.bytes))
        out.dimensions @?= Just (32, 16)
    , testCase "an upright orientation is not worth a block either" do
        out <- scrubbed (jpeg [jfif, exif 1] [])
        assertBool "no EXIF" (not ("Exif" `BS.isInfixOf` out.bytes))
    , testCase "a camera's JPEG with no JFIF header keeps its orientation" do
        out <- scrubbed (jpeg [exif 8] [])
        assertBool "EXIF directly after the start of image" (BS.pack [0xFF, 0xD8, 0xFF, 0xE1] `BS.isPrefixOf` out.bytes)
        out.dimensions @?= Just (16, 32)
    , testCase "scrubbing what was scrubbed changes nothing" do
        once <- scrubbed sampleJpeg
        twice <- scrubbed once.bytes
        twice @?= once
    , testCase "a second scan and the table between the scans are kept" do
        let second = segment 0xC4 (BS.replicate 9 3) <> segment 0xDA (BS.pack [1, 1, 0, 0, 63, 0]) <> BS.pack [0x77, 0x88]
        out <- scrubbed (jpeg [jfif] [second])
        assertBool "second scan" (second `BS.isInfixOf` out.bytes)
    , testCase "what is not a complete image is refused" do
        assertBool "cut off before the end of image" (isLeft (scrub (BS.dropEnd (2 + BS.length "TRAILERSECRET") (jpeg [jfif] []))))
        assertBool "cut off inside a segment" (isLeft (scrub (BS.take 12 sampleJpeg)))
        assertBool "no frame" (isLeft (scrub (BS.pack [0xFF, 0xD8] <> jfif <> BS.pack [0xFF, 0xD9])))
        assertBool "a segment that claims more bytes than there are" (isLeft (scrub (BS.pack [0xFF, 0xD8, 0xFF, 0xE1, 0xFF, 0xFF, 1, 2, 3])))
    , testCase "a side longer than the limit is refused" do
        let wide = BS.pack [0xFF, 0xD8] <> segment 0xC0 (BS.pack [8, 0, 16] <> be16 (maxSide + 1) <> BS.pack [1, 1, 0x11, 0]) <> scanAndEnd
        assertBool "refused" (isLeft (scrub wide))
    , testCase "a PNG keeps its picture chunks and nothing after the end chunk" do
        out <- scrubbed samplePng
        mapM_ (\secret -> assertBool (show secret <> " is gone") (not (secret `BS.isInfixOf` out.bytes))) secrets
        out.mime @?= "image/png"
        out.dimensions @?= Just (3, 2)
        out.bytes @?= pngSignature <> chunk "IHDR" ihdr <> chunk "gAMA" "gama" <> chunk "IDAT" "pixels" <> chunk "IEND" ""
    , testCase "a PNG with no end chunk, or no image data, is refused" do
        assertBool "no end" (isLeft (scrub (pngSignature <> chunk "IHDR" ihdr <> chunk "IDAT" "pixels")))
        assertBool "no data" (isLeft (scrub (pngSignature <> chunk "IHDR" ihdr <> chunk "IEND" "")))
        assertBool "header not first" (isLeft (scrub (pngSignature <> chunk "IDAT" "pixels" <> chunk "IHDR" ihdr <> chunk "IEND" "")))
    , testCase "anything else is not an image this server accepts" do
        scrub "GIF89a and so on" @?= Left UnknownFormat
        scrub "<html><script>alert(1)</script></html>" @?= Left UnknownFormat
        scrub "" @?= Left UnknownFormat
    , testProperty "whatever metadata segments are added, the result is the same file" $
        forAll (listOf droppable) \extras ->
          fmap (.bytes) (scrub (jpeg (jfif : extras) [])) === fmap (.bytes) (scrub (jpeg [jfif] []))
    ]
  where
    scrubbed input = either (\why -> assertFailure ("refused: " <> show why)) pure (scrub input)

{- | An application or comment segment of the kinds the scrubber drops,
with arbitrary content.
-}
droppable :: QC.Gen ByteString
droppable = do
  marker <- elements [0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEF, 0xFE]
  size <- QC.choose (0, 40)
  payload <- vectorOf size QC.arbitrary
  pure (segment marker (BS.pack payload))

{- | Strings planted in every place a file can carry something that is not
the picture. None may survive.
-}
secrets :: [ByteString]
secrets = ["THUMBSECRET", "GPSSECRET", "XMPSECRET", "IPTCSECRET", "COMMENTSECRET", "MPFSECRET", "TRAILERSECRET", "TEXTSECRET", "EXIFSECRET", "TIMESECRET"]

segment :: Word8 -> ByteString -> ByteString
segment marker payload = BS.pack [0xFF, marker] <> be16 (BS.length payload + 2) <> payload

be16 :: Int -> ByteString
be16 n = BS.pack [fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]

-- | A JFIF header that carries a thumbnail after its fixed fourteen bytes.
jfif :: ByteString
jfif = segment 0xE0 ("JFIF\0" <> BS.pack [1, 1, 0, 0, 1, 0, 1, 1, 1] <> "THUMBSECRET")

{- | A little-endian EXIF block with the given orientation and a pointer
to position data that follows it.
-}
exif :: Word8 -> ByteString
exif orientation =
  segment 0xE1 $
    "Exif\0\0"
      <> "II"
      <> BS.pack [0x2A, 0, 8, 0, 0, 0]
      <> BS.pack [2, 0]
      <> BS.pack [0x12, 0x01, 3, 0, 1, 0, 0, 0, orientation, 0, 0, 0]
      <> BS.pack [0x25, 0x88, 4, 0, 1, 0, 0, 0, 38, 0, 0, 0]
      <> BS.pack [0, 0, 0, 0]
      <> "GPSSECRET"

entropy :: ByteString
entropy = BS.pack [0x12, 0xFF, 0x00, 0x34, 0xFF, 0xD0, 0x56, 0xFF, 0x00]

-- | The first scan with its image data, then the end of image.
scanAndEnd :: ByteString
scanAndEnd = segment 0xDA (BS.pack [1, 1, 0, 0, 63, 0]) <> entropy <> BS.pack [0xFF, 0xD9]

{- | A JPEG 32 wide and 16 high: the given segments, the tables and frame,
one scan, anything given to follow that scan, the end of image, and
bytes after the end that belong to nothing.
-}
jpeg :: [ByteString] -> [ByteString] -> ByteString
jpeg before afterScan =
  BS.concat
    ( [BS.pack [0xFF, 0xD8]]
        <> before
        <> [ segment 0xDB (BS.replicate 65 1)
           , segment 0xC0 (BS.pack [8, 0, 16, 0, 32, 1, 1, 0x11, 0])
           , segment 0xC4 (BS.replicate 20 2)
           , segment 0xDA (BS.pack [1, 1, 0, 0, 63, 0])
           , entropy
           ]
        <> afterScan
        <> [BS.pack [0xFF, 0xD9], "TRAILERSECRET"]
    )

sampleJpeg :: ByteString
sampleJpeg =
  jpeg
    [ jfif
    , exif 6
    , segment 0xE1 "http://ns.adobe.com/xap/1.0/\0<x:xmpmeta>XMPSECRET"
    , segment 0xED "Photoshop 3.0\0IPTCSECRET"
    , segment 0xFE "COMMENTSECRET"
    , segment 0xE2 ("ICC_PROFILE\0" <> BS.pack [1, 1] <> "profile")
    , segment 0xE2 "MPF\0MPFSECRET"
    ]
    []

pngSignature :: ByteString
pngSignature = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

{- | A chunk with a checksum of zeros; the scrubber copies checksums and
does not verify them.
-}
chunk :: ByteString -> ByteString -> ByteString
chunk kind content = BS.pack [0, 0] <> be16 (BS.length content) <> kind <> content <> BS.pack [0, 0, 0, 0]

ihdr :: ByteString
ihdr = BS.pack [0, 0, 0, 3, 0, 0, 0, 2, 8, 2, 0, 0, 0]

samplePng :: ByteString
samplePng =
  BS.concat
    [ pngSignature
    , chunk "IHDR" ihdr
    , chunk "tEXt" "Comment\0TEXTSECRET"
    , chunk "gAMA" "gama"
    , chunk "eXIf" "EXIFSECRET"
    , chunk "iTXt" "XML:com.adobe.xmp\0XMPSECRET"
    , chunk "IDAT" "pixels"
    , chunk "tIME" "TIMESECRET"
    , chunk "IEND" ""
    , "TRAILERSECRET"
    ]
