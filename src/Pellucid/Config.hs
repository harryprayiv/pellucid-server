{- | What an operator sets, and the environment every request handler is
given.
-}
module Pellucid.Config (
  Config (..),
  Env (..),
  softwareName,
  softwareVersion,
  absoluteUrl,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Pellucid.Domain.Limits (Limits)
import Pellucid.Store (Db)

data Config = Config
  { baseUrl :: Text
  -- ^ Where clients reach this server, scheme and host with no trailing
  -- slash: @https://photos.example@. Every absolute URL the server writes
  -- (a Link header, a media URL, a profile URL) starts with it.
  , title :: Text
  , databaseUrl :: ByteString
  , mediaDir :: FilePath
  , port :: Int
  , limits :: Limits
  }

data Env = Env
  { config :: Config
  , db :: Db
  , clock :: IO UTCTime
  , decoyPasswordHash :: Text
  -- ^ The hash of a password nobody has, made at start. A sign-in for a
  -- username that does not exist is checked against it, so that the
  -- answer takes as long as it does for a wrong password and the time it
  -- takes does not say which usernames exist.
  }

{- | The name this server gives for itself in nodeinfo. It is its own name
(decision S17): a server that is not Pixelfed does not say it is.
-}
softwareName :: Text
softwareName = "pellucid"

softwareVersion :: Text
softwareVersion = "0.1.0"

absoluteUrl :: Config -> Text -> Text
absoluteUrl config path = T.dropWhileEnd (== '/') config.baseUrl <> path
