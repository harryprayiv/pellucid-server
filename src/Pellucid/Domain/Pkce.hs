{- | RFC 7636 as the server sees it: the challenge arrives with the
authorization request and the verifier with the token request. Only the
S256 method is accepted (rule AuthorizationGranted).
-}
module Pellucid.Domain.Pkce (
  Challenge,
  PkceProblem (..),
  mkChallenge,
  challengeText,
  challengeOf,
  verifierMatches,
) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | A stored S256 challenge. Only 'mkChallenge' makes one.
newtype Challenge = Challenge Text
  deriving stock (Eq, Show)

data PkceProblem
  = MethodNotS256
  | ChallengeMalformed
  deriving stock (Eq, Show)

{- | The challenge and its method as the authorization request carries them.
An S256 challenge is the unpadded base64url form of 32 bytes: 43
characters of that alphabet.
-}
mkChallenge :: Text -> Text -> Either PkceProblem Challenge
mkChallenge challenge method
  | method /= "S256" = Left MethodNotS256
  | T.length challenge /= 43 || not (T.all base64UrlChar challenge) = Left ChallengeMalformed
  | otherwise = Right (Challenge challenge)

challengeText :: Challenge -> Text
challengeText (Challenge c) = c

{- | What a client derives from its verifier: BASE64URL(SHA256(verifier)).
'Nothing' when the verifier is not 43 to 128 unreserved characters.
-}
challengeOf :: Text -> Maybe Challenge
challengeOf verifier
  | T.length verifier < 43 || T.length verifier > 128 = Nothing
  | not (T.all unreserved verifier) = Nothing
  | otherwise = Just (Challenge (TE.decodeUtf8 (Base64Url.encodeUnpadded (SHA256.hash (TE.encodeUtf8 verifier)))))
  where
    unreserved c = base64UrlChar c || c == '.' || c == '~'

verifierMatches :: Challenge -> Text -> Bool
verifierMatches challenge verifier = challengeOf verifier == Just challenge

base64UrlChar :: Char -> Bool
base64UrlChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '-' || c == '_'
