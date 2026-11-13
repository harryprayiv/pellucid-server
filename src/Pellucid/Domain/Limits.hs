{- | The numbers in the spec's @config@ block. They are published at the
instance endpoint so that a client can apply them before it sends
anything.
-}
module Pellucid.Domain.Limits (
  Limits (..),
  defaultLimits,
  maxUploadBytes,
) where

import Data.Text (Text)
import Data.Time (NominalDiffTime)

data Limits = Limits
  { maxCaptionLength :: Int
  , maxAlbumLength :: Int
  , maxAltTextLength :: Int
  , maxSpoilerLength :: Int
  , maxUploadKb :: Int
  , allowedMediaTypes :: [Text]
  , openRegistration :: Bool
  , authorizationCodeLifetime :: NominalDiffTime
  , accessTokenLifetime :: NominalDiffTime
  , unattachedMediaLifetime :: NominalDiffTime
  , listPage :: Int
  , listPageMax :: Int
  , contextMax :: Int
  -- ^ How many comments one request for a thread returns.
  }
  deriving stock (Eq, Show)

{- | The spec's defaults, with one difference that the spec's
implementation status records: only the media types that
"Pellucid.Domain.Scrub" can strip of metadata are accepted.
-}
defaultLimits :: Limits
defaultLimits =
  Limits
    { maxCaptionLength = 500
    , maxAlbumLength = 10
    , maxAltTextLength = 1000
    , maxSpoilerLength = 140
    , maxUploadKb = 15000
    , allowedMediaTypes = ["image/jpeg", "image/png"]
    , openRegistration = False
    , authorizationCodeLifetime = 10 * 60
    , accessTokenLifetime = 365 * 24 * 60 * 60
    , unattachedMediaLifetime = 24 * 60 * 60
    , listPage = 20
    , listPageMax = 40
    , contextMax = 100
    }

maxUploadBytes :: Limits -> Int
maxUploadBytes limits = limits.maxUploadKb * 1024
