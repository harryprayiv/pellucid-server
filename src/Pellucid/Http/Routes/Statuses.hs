-- | Posts and the two timelines.
module Pellucid.Http.Routes.Statuses (
  routes,
  listWindow,
) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (methodDelete, methodGet, methodPost, status200, status403, status501)
import Network.Wai qualified as Wai
import Pellucid.Config (Config (..), Env (..), absoluteUrl)
import Pellucid.Domain.Account (canAct)
import Pellucid.Domain.Cursor (Cursor (..), Page (..), clampLimit, decodeCursor, paginate)
import Pellucid.Domain.Ids (MediaId (..), PostId (..), parseId)
import Pellucid.Domain.Limits (Limits (..))
import Pellucid.Domain.OAuth (Need (..))
import Pellucid.Domain.Post (
  Attachment (..),
  MediaStatus (..),
  Submission (..),
  Visibility (..),
  canSee,
  checkPublish,
  describePublishProblem,
  mayWithdraw,
  visibilityFromApi,
 )
import Pellucid.Http.Entities (loadStatuses, viewerOf)
import Pellucid.Http.Params (BodyLimit (..), FieldProblem (..), Reader, bool, int, listOf, optionalField, text, withDefault)
import Pellucid.Http.Respond (Handler, invalid, json, notFound, pageLink, refuse)
import Pellucid.Http.Router (Access (..), Request (..), Route, Session (..), capture, parse, requireViewer, route)
import Pellucid.Store (withConnection)
import Pellucid.Store.Media (MediaRow (..), findMedia, mediaPath)
import Pellucid.Store.Posts (NewPost (..), PostRow (..), findPost, homeTimeline, insertPost, publicTimeline, withdrawPost)
import System.Directory (doesFileExist, removeFile)

routes :: [Route]
routes =
  [ route methodGet "/api/v1/timelines/home" (Signed NeedRead) SmallBody home
  , route methodGet "/api/v1/timelines/public" Open SmallBody public
  , route methodPost "/api/v1/statuses" (Signed NeedWrite) SmallBody publish
  , route methodGet "/api/v1/statuses/:id" Open SmallBody showStatus
  , route methodGet "/api/v1/statuses/:id/context" Open SmallBody context
  , route methodDelete "/api/v1/statuses/:id" (Signed NeedWrite) SmallBody withdraw
  ]

{- | Where in a list a request starts and how many items it wants (S12).
@cursor@ is the parameter the Link header carries. @max_id@ is what
older clients send and means the same thing: everything below this id.
A cursor that does not decode is a refusal, not the top of the list.
-}
listWindow :: Env -> Request -> Handler (Maybe Cursor, Int)
listWindow env request = do
  (cursor, maxId, limit) <-
    parse request ((,,) <$> optionalField "cursor" cursorReader <*> optionalField "max_id" idReader <*> optionalField "limit" int)
  let limits = env.config.limits
  pure (maybe (Cursor <$> maxId) Just cursor, clampLimit limits.listPage limits.listPageMax limit)
  where
    cursorReader :: Reader Cursor
    cursorReader raw = maybe (Left "is not a cursor this server issued") Right (decodeCursor raw)
    idReader raw = maybe (Left "is not an id") Right (parseId raw)

-- | Send a page of posts with the header that leads to the next one.
sendPage :: Env -> Text -> Int -> Page PostRow -> Connection -> IO Wai.Response
sendPage env path limit page conn = do
  entities <- loadStatuses env.config conn page.items
  let base = absoluteUrl env.config (path <> "?limit=" <> T.pack (show limit))
  pure (json status200 (pageLink base page.next) (entities :: [Value]))

home :: Env -> Request -> Handler Wai.Response
home env request = do
  session <- requireViewer request
  (cursor, limit) <- listWindow env request
  liftIO . withConnection env.db $ \conn -> do
    rows <- homeTimeline conn session.account cursor (limit + 1)
    sendPage env "/api/v1/timelines/home" limit (paginate limit (\row -> let PostId n = row.postId in n) rows) conn

public :: Env -> Request -> Handler Wai.Response
public env request = do
  (cursor, limit) <- listWindow env request
  liftIO . withConnection env.db $ \conn -> do
    rows <- publicTimeline conn cursor (limit + 1)
    sendPage env "/api/v1/timelines/public" limit (paginate limit (\row -> let PostId n = row.postId in n) rows) conn

-- | What a client sends to publish.
data Draft = Draft
  { caption :: Text
  , mediaIds :: [Text]
  , visibility :: Maybe Text
  , sensitive :: Bool
  , spoilerText :: Text
  , commentsDisabled :: Bool
  , inReplyTo :: Maybe Text
  }

-- | Rule PostPublished.
publish :: Env -> Request -> Handler Wai.Response
publish env request = do
  session <- requireViewer request
  let limits = env.config.limits
  draft <-
    parse request $
      Draft
        <$> withDefault "" "status" text
        <*> listOf "media_ids" text
        <*> optionalField "visibility" text
        <*> withDefault False "sensitive" bool
        <*> withDefault "" "spoiler_text" text
        <*> withDefault False "comments_disabled" bool
        <*> optionalField "in_reply_to_id" text
  when (isJust draft.inReplyTo) $
    throwE (refuse status501 "comments_not_built" "comments are not built yet; a post with in_reply_to_id is refused, not published as something else")
  visibility <- case draft.visibility of
    Nothing -> pure Public
    Just raw -> maybe (throwE (invalid [FieldProblem "visibility" "is not public, unlisted, private or direct"])) pure (visibilityFromApi raw)
  mediaIds <- case traverse parseId draft.mediaIds of
    Just ids -> pure (map MediaId ids)
    Nothing -> throwE (invalid [FieldProblem "media_ids" "holds something that is not an id"])
  when (length mediaIds /= length (unique mediaIds)) $
    throwE (invalid [FieldProblem "media_ids" "names an upload twice"])
  now <- liftIO env.clock
  result <- liftIO . withConnection env.db $ \conn -> do
    found <- mapM (findMedia conn) mediaIds
    -- An id that names no upload counts as one that cannot be used.
    let
      missing = Attachment {ownedByAuthor = False, status = Purged}
      attachments = [maybe missing (\row -> Attachment {ownedByAuthor = row.owner == session.account, status = row.status}) m | m <- found]
      submission = Submission {caption = draft.caption, spoilerText = draft.spoilerText, visibility, attachments}
    case checkPublish limits session.accountStatus submission of
      [] -> do
        created <-
          insertPost
            conn
            NewPost
              { author = session.account
              , caption = draft.caption
              , visibility
              , sensitive = draft.sensitive
              , spoilerText = draft.spoilerText
              , commentsDisabled = draft.commentsDisabled
              , publishedAt = now
              }
            mediaIds
        case created of
          Nothing -> pure (Left [FieldProblem "media_ids" "every attachment must be an upload of yours that is not yet on a post"])
          Just post -> do
            row <- findPost conn post
            Right <$> loadStatuses env.config conn (maybe [] pure row)
      problems -> pure (Left [FieldProblem field why | (field, why) <- map describePublishProblem problems])
  case result of
    Right (entity : _) -> pure (json status200 [] entity)
    Right [] -> throwE notFound
    Left problems
      | not (canAct session.accountStatus) -> throwE (refuse status403 "account_not_active" "this account cannot post while it is suspended or leaving")
      | otherwise -> throwE (invalid problems)
  where
    unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

{- | A post the caller can see (guarantee ReadsObeyTheReadingRules), with
the connection it was found on. One the caller cannot see is the same
missing page as one that does not exist.
-}
visiblePost :: Env -> Request -> (Connection -> PostRow -> IO a) -> Handler a
visiblePost env request use = do
  raw <- capture 0 request
  wanted <- maybe (throwE notFound) (pure . PostId) (parseId raw)
  found <- liftIO . withConnection env.db $ \conn -> do
    row <- findPost conn wanted
    case row of
      Nothing -> pure Nothing
      Just post -> do
        viewer <- viewerOf conn request.viewer post.author
        if canSee post.status post.visibility viewer
          then Just <$> use conn post
          else pure Nothing
  maybe (throwE notFound) pure found

showStatus :: Env -> Request -> Handler Wai.Response
showStatus env request = do
  entities <- visiblePost env request (\conn post -> loadStatuses env.config conn [post])
  case entities of
    entity : _ -> pure (json status200 [] entity)
    [] -> throwE notFound

{- | The posts above and below a post in its thread. Comments are not
built, so both lists are empty, which is also the true answer.
-}
context :: Env -> Request -> Handler Wai.Response
context env request = do
  () <- visiblePost env request (\_ _ -> pure ())
  pure (json status200 [] (object ["ancestors" .= ([] :: [Value]), "descendants" .= ([] :: [Value])]))

{- | Rule PostWithdrawn. The answer is the post as it was, which is what
clients of this API expect from a delete.
-}
withdraw :: Env -> Request -> Handler Wai.Response
withdraw env request = do
  session <- requireViewer request
  raw <- capture 0 request
  wanted <- maybe (throwE notFound) (pure . PostId) (parseId raw)
  outcome <- liftIO . withConnection env.db $ \conn -> do
    row <- findPost conn wanted
    case row of
      Just post | mayWithdraw (post.author == session.account) post.status -> do
        entities <- loadStatuses env.config conn [post]
        released <- withdrawPost conn wanted session.account
        pure (fmap (entities,) released)
      _ -> pure Nothing
  case outcome of
    Just (entity : _, released) -> do
      liftIO . forM_ released $ \media -> do
        let path = mediaPath env.config.mediaDir media
        present <- doesFileExist path
        when present (removeFile path)
      pure (json status200 [] entity)
    _ -> throwE notFound
