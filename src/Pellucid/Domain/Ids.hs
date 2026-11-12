{- | Identifiers. Each kind of thing has its own type so that an account id
cannot be passed where a post id is wanted. On the wire every id is a
string of decimal digits: an id is a 64-bit number, which JavaScript
cannot hold exactly, so it is never sent as a JSON number.
-}
module Pellucid.Domain.Ids (
  AccountId (..),
  PostId (..),
  MediaId (..),
  idText,
  parseId,
) where

import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T

newtype AccountId = AccountId Int64
  deriving stock (Eq, Ord, Show)

newtype PostId = PostId Int64
  deriving stock (Eq, Ord, Show)

newtype MediaId = MediaId Int64
  deriving stock (Eq, Ord, Show)

idText :: Int64 -> Text
idText = T.pack . show

{- | A positive 64-bit number written in decimal digits and nothing else.
Eighteen digits is the longest string that fits whatever its digits are;
nineteen is allowed when the number does.
-}
parseId :: Text -> Maybe Int64
parseId t
  | T.null t || T.length t > 19 || not (T.all isDigit t) = Nothing
  | otherwise =
      let n = T.foldl' (\acc c -> acc * 10 + toInteger (fromEnum c - fromEnum '0')) 0 t
       in if n >= 1 && n <= toInteger (maxBound :: Int64) then Just (fromInteger n) else Nothing
