-- | What the server says about itself before anyone signs in.
module Pellucid.Http.Routes.Instance (
  routes,
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types (methodGet, status200)
import Network.Wai qualified as Wai
import Pellucid.Config (Config (..), Env (..), absoluteUrl, softwareName, softwareVersion)
import Pellucid.Domain.Limits (Limits (..))
import Pellucid.Http.Params (BodyLimit (..))
import Pellucid.Http.Respond (Handler, json)
import Pellucid.Http.Router (Access (..), Request, Route, route)
import Pellucid.Store (withConnection)
import Pellucid.Store.Accounts (countLocalAccounts)
import Pellucid.Store.Posts (countLocalPosts)

routes :: [Route]
routes =
  [ route methodGet "/.well-known/nodeinfo" Open SmallBody wellKnown
  , route methodGet "/api/nodeinfo/2.0.json" Open SmallBody nodeinfo
  , route methodGet "/api/v1/instance" Open SmallBody instanceV1
  , route methodGet "/api/v1.1/compose/settings" Open SmallBody composeSettings
  ]

wellKnown :: Env -> Request -> Handler Wai.Response
wellKnown env _ =
  pure . json status200 [] $
    object
      [ "links"
          .= [ object
                [ "rel" .= ("http://nodeinfo.diaspora.software/ns/schema/2.0" :: Text)
                , "href" .= absoluteUrl env.config "/api/nodeinfo/2.0.json"
                ]
             ]
      ]

{- | The software's own name and version (decision S17), and two counts.
The protocol list is empty because the server does not federate yet;
the schema asks for at least one entry, and a protocol this server does
not speak is not put there to satisfy it.
-}
nodeinfo :: Env -> Request -> Handler Wai.Response
nodeinfo env _ = do
  (accounts, posts) <- liftIO (withConnection env.db \conn -> (,) <$> countLocalAccounts conn <*> countLocalPosts conn)
  pure . json status200 [] $
    object
      [ "version" .= ("2.0" :: Text)
      , "software" .= object ["name" .= softwareName, "version" .= softwareVersion]
      , "protocols" .= ([] :: [Text])
      , "services" .= object ["inbound" .= ([] :: [Text]), "outbound" .= ([] :: [Text])]
      , "openRegistrations" .= env.config.limits.openRegistration
      , "usage" .= object ["users" .= object ["total" .= accounts], "localPosts" .= posts]
      , "metadata" .= object ["nodeName" .= env.config.title]
      ]

instanceV1 :: Env -> Request -> Handler Wai.Response
instanceV1 env _ = do
  (accounts, posts) <- liftIO (withConnection env.db \conn -> (,) <$> countLocalAccounts conn <*> countLocalPosts conn)
  let limits = env.config.limits
  pure . json status200 [] $
    object
      [ "uri" .= host env.config.baseUrl
      , "title" .= env.config.title
      , "short_description" .= ("" :: Text)
      , "description" .= ("" :: Text)
      , "email" .= ("" :: Text)
      , "version" .= softwareVersion
      , "urls" .= object []
      , "stats" .= object ["user_count" .= accounts, "status_count" .= posts, "domain_count" .= (0 :: Int)]
      , "thumbnail" .= Null
      , "languages" .= (["en"] :: [Text])
      , "registrations" .= limits.openRegistration
      , "approval_required" .= False
      , "contact_account" .= Null
      , "rules" .= ([] :: [Value])
      , "configuration"
          .= object
            [ "statuses"
                .= object
                  [ "max_characters" .= limits.maxCaptionLength
                  , "max_media_attachments" .= limits.maxAlbumLength
                  ]
            , "media_attachments"
                .= object
                  [ "supported_mime_types" .= limits.allowedMediaTypes
                  , "image_size_limit" .= (limits.maxUploadKb * 1024)
                  ]
            ]
      ]
  where
    host url = fromMaybe url (T.stripPrefix "https://" url <|> T.stripPrefix "http://" url)

-- | The limits a composer applies before it sends anything.
composeSettings :: Env -> Request -> Handler Wai.Response
composeSettings env _ =
  let limits = env.config.limits
   in pure . json status200 [] $
        object
          [ "allowed_media_types" .= limits.allowedMediaTypes
          , "max_caption_length" .= limits.maxCaptionLength
          , "max_file_size" .= limits.maxUploadKb
          , "max_media_attachments" .= limits.maxAlbumLength
          , "max_altext_length" .= limits.maxAltTextLength
          , "default_scope" .= ("public" :: Text)
          ]
