-- | An account's notifications.
module Pellucid.Http.Routes.Notifications (
  routes,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Text qualified as T
import Network.HTTP.Types (methodGet, status200)
import Network.Wai qualified as Wai
import Pellucid.Config (Env (..), absoluteUrl)
import Pellucid.Domain.Cursor (Page (..), paginate)
import Pellucid.Domain.OAuth (Need (..))
import Pellucid.Http.Entities (loadNotifications)
import Pellucid.Http.Params (BodyLimit (..))
import Pellucid.Http.Respond (Handler, json, pageLink)
import Pellucid.Http.Router (Access (..), Request, Route, Session (..), requireViewer, route)
import Pellucid.Http.Routes.Statuses (listWindow)
import Pellucid.Store (withConnection)
import Pellucid.Store.Notifications (NotificationRow (..), markSeen, notificationsFor)

routes :: [Route]
routes =
  [ route methodGet "/api/v1/notifications" (Signed NeedRead) SmallBody list
  ]

{- | The caller's notifications, newest first. Reading a page is what
marks its notifications seen (rule NotificationSeen): this API has no
separate call for it.
-}
list :: Env -> Request -> Handler Wai.Response
list env request = do
  session <- requireViewer request
  (cursor, limit) <- listWindow env request
  (page, entities) <- liftIO . withConnection env.db $ \conn -> do
    rows <- notificationsFor conn session.account cursor (limit + 1)
    let page = paginate limit (.notificationId) rows
    entities <- loadNotifications env.config conn session.account page.items
    markSeen conn session.account (map (.notificationId) page.items)
    pure (page, entities)
  let base = absoluteUrl env.config ("/api/v1/notifications?limit=" <> T.pack (show limit))
  pure (json status200 (pageLink base page.next) (entities :: [Value]))
