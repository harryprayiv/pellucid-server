-- | Posts, comments, threads and the two timelines.
module Pellucid.Http.Routes.Statuses (
  routes,
  listWindow,
  Reach (..),
  visiblePost,
  readerOf,
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (methodDelete, methodGet, methodPost, status200, status403)
import Network.Wai qualified as Wai
import Pellucid.Config (Config (..), Env (..), absoluteUrl)
import Pellucid.Domain.Account (canAct)
import Pellucid.Domain.Cursor (Cursor (..), Page (..), clampLimit, decodeCursor, paginate)
import Pellucid.Domain.Ids (AccountId, MediaId (..), PostId (..), parseId)
import Pellucid.Domain.Interaction (CommentProblem (..), CommentSubmission (..), checkComment, describeCommentProblem, recipientOf)
import Pellucid.Domain.Limits (Limits (..))
import Pellucid.Domain.OAuth (Need (..))
import Pellucid.Domain.Post (
  Attachment (..),
  MediaStatus (..),
  PostStatus (..),
  Submission (..),
  Visibility (..),
  canSee,
  canSeeComment,
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
import Pellucid.Store.Posts (
  NewComment (..),
  NewPost (..),
  PostRow (..),
  ancestorsOf,
  descendantsOf,
  findPost,
  homeTimeline,
  insertComment,
  insertPost,
  publicTimeline,
  withdrawPost,
 )
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

-- | The account a request speaks for, if it carries a token.
readerOf :: Request -> Maybe AccountId
readerOf request = fmap (.account) request.viewer

-- | Send a page of posts with the header that leads to the next one.
sendPage :: Env -> Request -> Text -> Int -> Page PostRow -> Connection -> IO Wai.Response
sendPage env request path limit page conn = do
  entities <- loadStatuses env.config conn (readerOf request) page.items
  let base = absoluteUrl env.config (path <> "?limit=" <> T.pack (show limit))
  pure (json status200 (pageLink base page.next) (entities :: [Value]))

home :: Env -> Request -> Handler Wai.Response
home env request = do
  session <- requireViewer request
  (cursor, limit) <- listWindow env request
  liftIO . withConnection env.db $ \conn -> do
    rows <- homeTimeline conn session.account cursor (limit + 1)
    sendPage env request "/api/v1/timelines/home" limit (paginate limit (\row -> let PostId n = row.postId in n) rows) conn

public :: Env -> Request -> Handler Wai.Response
public env request = do
  (cursor, limit) <- listWindow env request
  liftIO . withConnection env.db $ \conn -> do
    rows <- publicTimeline conn cursor (limit + 1)
    sendPage env request "/api/v1/timelines/public" limit (paginate limit (\row -> let PostId n = row.postId in n) rows) conn

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

{- | Rule PostPublished, or rule CommentPublished when the request names
what it answers.
-}
publish :: Env -> Request -> Handler Wai.Response
publish env request = do
  session <- requireViewer request
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
  case draft.inReplyTo of
    Just parent -> publishComment env session draft parent
    Nothing -> publishPost env session draft

publishPost :: Env -> Session -> Draft -> Handler Wai.Response
publishPost env session draft = do
  let limits = env.config.limits
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
            Right <$> loadStatuses env.config conn (Just session.account) (maybe [] pure row)
      problems -> pure (Left [FieldProblem field why | (field, why) <- map describePublishProblem problems])
  case result of
    Right (entity : _) -> pure (json status200 [] entity)
    Right [] -> throwE notFound
    Left problems
      | not (canAct session.accountStatus) -> throwE (refuse status403 "account_not_active" "this account cannot post while it is suspended or leaving")
      | otherwise -> throwE (invalid problems)
  where
    unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

{- | Rule CommentPublished. A comment takes its parent's visibility and
carries no attachment, so a request that names both a parent and an
upload is refused instead of losing the upload silently. A parent the
caller cannot read is answered the same way as one that does not exist.
-}
publishComment :: Env -> Session -> Draft -> Text -> Handler Wai.Response
publishComment env session draft rawParent = do
  parentId <- maybe (throwE (invalid [describe ParentOutOfReach])) (pure . PostId) (parseId rawParent)
  unless (null draft.mediaIds) $
    throwE (invalid [FieldProblem "media_ids" "a comment carries no attachment"])
  now <- liftIO env.clock
  result <- liftIO . withConnection env.db $ \conn -> do
    parent <- findPost conn parentId
    reach <- maybe (pure Reach {post = False, thread = False}) (reachOf conn (Just session)) parent
    let submission =
          CommentSubmission
            { authorStatus = session.accountStatus
            , parentReadable = reach.post && fmap (.status) parent == Just Published
            , commentsDisabled = maybe False (.commentsDisabled) parent
            , text = draft.caption
            }
    case (checkComment env.config.limits submission, parent) of
      ([], Just row) -> do
        created <-
          insertComment
            conn
            NewComment {author = session.account, text = draft.caption, parent = parentId, publishedAt = now}
            (recipientOf session.account row.author)
        case created of
          Nothing -> pure (Left [ParentOutOfReach])
          Just comment -> do
            found <- findPost conn comment
            Right <$> loadStatuses env.config conn (Just session.account) (maybe [] pure found)
      ([], Nothing) -> pure (Left [ParentOutOfReach])
      (problems, _) -> pure (Left problems)
  case result of
    Right (entity : _) -> pure (json status200 [] entity)
    Right [] -> throwE notFound
    Left problems
      | CommenterNotActive `elem` problems -> throwE (refuse status403 "account_not_active" "this account cannot comment while it is suspended or leaving")
      | otherwise -> throwE (invalid (map describe problems))
  where
    describe problem = let (field, why) = describeCommentProblem problem in FieldProblem field why

{- | What a caller may read of a post: the post itself, and the thread it
is in. The two differ for a comment under a post the caller cannot read,
which its own author still reads.
-}
data Reach = Reach
  { post :: Bool
  , thread :: Bool
  }

{- | The reading rules for one post. A post is read under its own
visibility. For a comment the question is asked of the post at the top
of its thread.
-}
reachOf :: Connection -> Maybe Session -> PostRow -> IO Reach
reachOf conn viewer row = do
  toAuthor <- viewerOf conn viewer row.author
  case row.threadRoot of
    Nothing -> do
      let readable = canSee row.status row.visibility toAuthor
      pure Reach {post = readable, thread = readable}
    Just top -> do
      found <- findPost conn top
      thread <- case found of
        Nothing -> pure False
        Just topRow -> do
          toTop <- viewerOf conn viewer topRow.author
          pure (canSee topRow.status topRow.visibility toTop)
      pure Reach {post = canSeeComment row.status toAuthor thread, thread}

{- | A post the caller can see (guarantee ReadsObeyTheReadingRules), with
the connection it was found on and what the caller may read around it.
One the caller cannot see is the same missing page as one that does not
exist.
-}
visiblePost :: Env -> Request -> (Connection -> PostRow -> Reach -> IO a) -> Handler a
visiblePost env request use = do
  raw <- capture 0 request
  wanted <- maybe (throwE notFound) (pure . PostId) (parseId raw)
  found <- liftIO . withConnection env.db $ \conn -> do
    row <- findPost conn wanted
    case row of
      Nothing -> pure Nothing
      Just found -> do
        reach <- reachOf conn request.viewer found
        if reach.post
          then Just <$> use conn found reach
          else pure Nothing
  maybe (throwE notFound) pure found

showStatus :: Env -> Request -> Handler Wai.Response
showStatus env request = do
  entities <- visiblePost env request (\conn found _ -> loadStatuses env.config conn (readerOf request) [found])
  case entities of
    entity : _ -> pure (json status200 [] entity)
    [] -> throwE notFound

{- | What a post answers and what answers it. The comments under it come
oldest first, at any depth, each naming what it answers, and there are
at most @context_max@ of them. A caller who reads a comment of their
own under a post they can no longer read is shown their own part of the
thread and nothing else.
-}
context :: Env -> Request -> Handler Wai.Response
context env request = do
  (above, under) <- visiblePost env request \conn found reach -> do
    let keep = filter (\row -> reach.thread || Just row.author == readerOf request)
    ancestors <- keep <$> ancestorsOf conn found.postId
    descendants <- keep <$> descendantsOf conn found.postId env.config.limits.contextMax
    (,) <$> loadStatuses env.config conn (readerOf request) ancestors <*> loadStatuses env.config conn (readerOf request) descendants
  pure (json status200 [] (object ["ancestors" .= (above :: [Value]), "descendants" .= (under :: [Value])]))

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
        entities <- loadStatuses env.config conn (Just session.account) [post]
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
