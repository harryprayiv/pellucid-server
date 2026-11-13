{- | Likes and bookmarks: placing one, taking one back, who likes a post,
and the two lists an account keeps.
-}
module Pellucid.Http.Routes.Interactions (
  routes,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (methodGet, methodPost, status200, status403)
import Network.Wai qualified as Wai
import Pellucid.Config (Env (..), absoluteUrl)
import Pellucid.Domain.Cursor (Cursor, Page (..), paginate)
import Pellucid.Domain.Ids (AccountId, PostId (..), idText)
import Pellucid.Domain.Interaction (Mark (..), MarkOutcome (..), MarkRefusal (..), decideBookmark, decideLike, recipientOf)
import Pellucid.Domain.OAuth (Need (..))
import Pellucid.Domain.Post (PostStatus (..))
import Pellucid.Http.Entities (accountJson, loadStatuses)
import Pellucid.Http.Params (BodyLimit (..))
import Pellucid.Http.Respond (Handler, json, notFound, pageLink, refuse)
import Pellucid.Http.Router (Access (..), Request (..), Route, Session (..), requireViewer, route)
import Pellucid.Http.Routes.Statuses (Reach (..), listWindow, readerOf, visiblePost)
import Pellucid.Store (withConnection)
import Pellucid.Store.Accounts (AccountRow (..), findAccounts)
import Pellucid.Store.Interactions (
  bookmarkedPosts,
  deleteBookmark,
  deleteLike,
  hasBookmarked,
  hasLiked,
  insertBookmark,
  insertLike,
  likedPosts,
  likersOf,
 )
import Pellucid.Store.Posts (PostRow (..))

routes :: [Route]
routes =
  [ route methodPost "/api/v1/statuses/:id/favourite" (Signed NeedWrite) SmallBody like
  , route methodPost "/api/v1/statuses/:id/unfavourite" (Signed NeedWrite) SmallBody unlike
  , route methodPost "/api/v1/statuses/:id/bookmark" (Signed NeedWrite) SmallBody bookmark
  , route methodPost "/api/v1/statuses/:id/unbookmark" (Signed NeedWrite) SmallBody unbookmark
  , route methodGet "/api/v1/statuses/:id/favourited_by" Open SmallBody likers
  , route methodGet "/api/v1/favourites" (Signed NeedRead) SmallBody favourites
  , route methodGet "/api/v1/bookmarks" (Signed NeedRead) SmallBody bookmarks
  ]

{- | Find the post a path names, as the caller may read it, change
something about it, and answer with the post as it then is. Every route
that marks a post or takes a mark back has this shape. A post reaches
the change only if the caller can see it, so what is left of the rules'
reading requirement is that the post is published: its author and a
moderator can see one that has been taken down, and neither can like it.
-}
onPost :: Env -> Request -> (Connection -> Session -> PostRow -> IO (Either MarkRefusal ())) -> Handler Wai.Response
onPost env request change = do
  session <- requireViewer request
  outcome <- visiblePost env request \conn found _ -> do
    changed <- change conn session found
    case changed of
      Left refusal -> pure (Left refusal)
      Right () -> Right <$> loadStatuses env.config conn (Just session.account) [found]
  case outcome of
    Right (entity : _) -> pure (json status200 [] entity)
    Right [] -> throwE notFound
    Left PostOutOfReach -> throwE notFound
    Left ActorNotActive -> throwE (refuse status403 "account_not_active" "this account cannot like anything while it is suspended or leaving")

{- | Rule PostLiked. The post's author is told, unless the author is the
one who likes it.
-}
like :: Env -> Request -> Handler Wai.Response
like env request = do
  now <- liftIO env.clock
  onPost env request \conn session found -> do
    standing <- hasLiked conn session.account found.postId
    case decideLike Mark {actorStatus = session.accountStatus, readable = found.status == Published, standing} of
      Left refusal -> pure (Left refusal)
      Right AlreadyPlaced -> pure (Right ())
      Right Place -> Right <$> insertLike conn session.account found.postId (recipientOf session.account found.author) now

-- | Rule PostUnliked. Taking back a like that is not there changes nothing.
unlike :: Env -> Request -> Handler Wai.Response
unlike env request = onPost env request \conn session found -> Right <$> deleteLike conn session.account found.postId

-- | Rule PostBookmarked.
bookmark :: Env -> Request -> Handler Wai.Response
bookmark env request = onPost env request \conn session found -> do
  standing <- hasBookmarked conn session.account found.postId
  case decideBookmark Mark {actorStatus = session.accountStatus, readable = found.status == Published, standing} of
    Left refusal -> pure (Left refusal)
    Right AlreadyPlaced -> pure (Right ())
    Right Place -> Right <$> insertBookmark conn session.account found.postId

-- | Rule PostUnbookmarked.
unbookmark :: Env -> Request -> Handler Wai.Response
unbookmark env request = onPost env request \conn session found -> Right <$> deleteBookmark conn session.account found.postId

{- | Who likes a post, newest like first, for anyone who can read the post
(decision S21).
-}
likers :: Env -> Request -> Handler Wai.Response
likers env request = do
  (cursor, limit) <- listWindow env request
  (path, page, accounts) <- visiblePost env request \conn found (_ :: Reach) -> do
    rows <- likersOf conn found.postId cursor (limit + 1)
    let page = paginate limit fst rows
    accounts <- findAccounts conn (map snd page.items)
    let PostId n = found.postId
    pure ("/api/v1/statuses/" <> idText n <> "/favourited_by", page, accounts)
  let inOrder = [accountJson env.config row | (_, account) <- page.items, row <- accounts, row.accountId == account]
  pure (json status200 (pageLink (listBase env path limit) page.next) (inOrder :: [Value]))

-- | The posts the caller likes and can still read, newest like first.
favourites :: Env -> Request -> Handler Wai.Response
favourites env request = markedList env request "/api/v1/favourites" likedPosts

-- | The posts the caller has bookmarked and can still read.
bookmarks :: Env -> Request -> Handler Wai.Response
bookmarks env request = markedList env request "/api/v1/bookmarks" bookmarkedPosts

markedList :: Env -> Request -> Text -> (Connection -> AccountId -> Maybe Cursor -> Int -> IO [(Int64, PostRow)]) -> Handler Wai.Response
markedList env request path list = do
  session <- requireViewer request
  (cursor, limit) <- listWindow env request
  (page, entities) <- liftIO . withConnection env.db $ \conn -> do
    rows <- list conn session.account cursor (limit + 1)
    let page = paginate limit fst rows
    entities <- loadStatuses env.config conn (readerOf request) (map snd page.items)
    pure (page, entities)
  pure (json status200 (pageLink (listBase env path limit) page.next) (entities :: [Value]))

listBase :: Env -> Text -> Int -> Text
listBase env path limit = absoluteUrl env.config (path <> "?limit=" <> T.pack (show limit))
