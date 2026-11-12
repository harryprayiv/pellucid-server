{- | Secrets: making them, and the only forms they are kept in.

A token, an authorization code and a client secret are 32 random bytes
from the operating system, handed out once as text and stored as their
SHA-256. They are long and random, so a fast hash is enough: there is
nothing to guess. A password is chosen by a person and is stored as an
Argon2id hash (surface SignIn, guarantee SecretsAreNeverReturnedTwice).
-}
module Pellucid.Secret (
  newSecret,
  hashSecret,
  hashPassword,
  passwordMatches,
) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Password.Argon2 qualified as Argon2
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import System.Entropy (getEntropy)

-- | 256 bits, as 43 characters that are safe in a URL and in a header.
newSecret :: IO Text
newSecret = TE.decodeUtf8 . Base64Url.encodeUnpadded <$> getEntropy 32

-- | The form a secret is stored and looked up in.
hashSecret :: Text -> Text
hashSecret = TE.decodeUtf8 . Base16.encode . SHA256.hash . TE.encodeUtf8

hashPassword :: Text -> IO Text
hashPassword password = Argon2.unPasswordHash <$> Argon2.hashPassword (Argon2.mkPassword password)

passwordMatches :: Text -> Text -> Bool
passwordMatches password stored =
  Argon2.checkPassword (Argon2.mkPassword password) (Argon2.PasswordHash stored) == Argon2.PasswordCheckSuccess
