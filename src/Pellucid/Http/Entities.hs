{- | The JSON this server sends for an account, an attachment, a post and a
relationship. The shapes are the ones the Pixelfed client API uses
(contract ClientApiCompat), and there is one of each: every route that
returns a post returns this one (invariant SameEntityEverywhere).

Every id is a string, every flag is a JSON boolean and every count is a
JSON number. Pixelfed sends some of these as they sit in its database;
this server does not.
-}
module Pellucid.Http.Entities (
  accountJson,
  mediaJson,
  statusJson,
  relationshipJson,
  loadStatuses,
  viewerOf,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Pellucid.Config (Config, absoluteUrl)
import Pellucid.Domain.Follow (FollowState (..))
import Pellucid.Domain.Ids (AccountId (..), MediaId (..), PostId (..), idText)
import Pellucid.Domain.Post (Viewer (..), visibilityToApi)
import Pellucid.Http.Respond (escapeHtml, timestamp)
import Pellucid.Http.Router (Session (..))
import Pellucid.Store.Accounts (AccountRow (..), FollowRow (..), findAccounts, followState)
import Pellucid.Store.Media (MediaRow (..), mediaOfPosts)
import Pellucid.Store.Posts (PostRow (..))

accountJson :: Config -> AccountRow -> Value
accountJson config row =
  object
    [ "id" .= idText accountNumber
    , "username" .= row.username
    , "acct" .= maybe row.username (\home -> row.username <> "@" <> home) row.home
    , "display_name" .= row.displayName
    , "note" .= paragraphs row.note
    , "note_text" .= row.note
    , "url" .= absoluteUrl config ("/users/" <> row.username)
    , "avatar" .= avatar
    , "avatar_static" .= avatar
    , "header" .= Null
    , "header_static" .= Null
    , "header_bg" .= Null
    , "locked" .= row.locked
    , "followers_count" .= row.followersCount
    , "following_count" .= row.followingCount
    , "statuses_count" .= row.postsCount
    , "local" .= (row.home == Nothing)
    , "is_admin" .= row.isModerator
    , "created_at" .= timestamp row.createdAt
    , "website" .= Null
    , "pronouns" .= ([] :: [Text])
    , "has_story" .= False
    , "bot" .= False
    , "emojis" .= ([] :: [Value])
    , "fields" .= ([] :: [Value])
    ]
  where
    AccountId accountNumber = row.accountId
    avatar = absoluteUrl config "/avatar/default.svg"

mediaJson :: Config -> MediaRow -> Value
mediaJson config row =
  object
    [ "id" .= idText mediaNumber
    , "type" .= ("image" :: Text)
    , "url" .= url
    , "preview_url" .= url
    , "optimized_url" .= Null
    , "remote_url" .= Null
    , "text_url" .= Null
    , "meta" .= meta
    , "description" .= (if T.null row.altText then Null else String row.altText)
    , "blurhash" .= Null
    , "is_nsfw" .= False
    , "mime" .= row.mime
    , "license" .= Null
    ]
  where
    MediaId mediaNumber = row.mediaId
    url = absoluteUrl config ("/media/" <> row.storageKey <> "." <> row.extension)
    meta = case (row.width, row.height) of
      (Just width, Just height)
        | width > 0 && height > 0 ->
            object
              [ "focus" .= object ["x" .= (0 :: Int), "y" .= (0 :: Int)]
              , "original"
                  .= object
                    [ "width" .= width
                    , "height" .= height
                    , "size" .= (T.pack (show width) <> "x" <> T.pack (show height))
                    , "aspect" .= (fromIntegral width / fromIntegral height :: Double)
                    ]
              ]
      _ -> Null

-- | A post with its author and its attachments.
statusJson :: Config -> AccountRow -> [MediaRow] -> PostRow -> Value
statusJson config author media row =
  object
    [ "id" .= idText postNumber
    , "uri" .= url
    , "url" .= url
    , "created_at" .= timestamp row.publishedAt
    , "edited_at" .= Null
    , "account" .= accountJson config author
    , "content" .= paragraphs row.caption
    , "content_text" .= row.caption
    , "visibility" .= visibilityToApi row.visibility
    , "sensitive" .= row.sensitive
    , "spoiler_text" .= row.spoilerText
    , "pf_type" .= kind
    , "media_attachments" .= map (mediaJson config) media
    , "favourites_count" .= (0 :: Int)
    , "reblogs_count" .= (0 :: Int)
    , "reply_count" .= row.replyCount
    , "favourited" .= False
    , "reblogged" .= False
    , "bookmarked" .= False
    , "comments_disabled" .= row.commentsDisabled
    , "in_reply_to_id" .= fmap (\(PostId parent) -> idText parent) row.inReplyTo
    , "in_reply_to_account_id" .= Null
    , "reblog" .= Null
    , "tags" .= ([] :: [Value])
    , "mentions" .= ([] :: [Value])
    , "emojis" .= ([] :: [Value])
    , "liked_by" .= object ["username" .= Null, "others" .= False]
    , "place" .= Null
    , "local" .= (author.home == Nothing)
    , "pinned" .= False
    ]
  where
    PostId postNumber = row.postId
    url = absoluteUrl config ("/p/" <> author.username <> "/" <> idText postNumber)
    kind :: Text
    kind = case (row.inReplyTo, length media) of
      (Just _, _) -> "text"
      (Nothing, 1) -> "photo"
      (Nothing, _) -> "photo:album"

{- | Plain text as the HTML the API calls @content@: escaped, with line
breaks kept. Nothing a person typed reaches a client as markup.
-}
paragraphs :: Text -> Text
paragraphs plain
  | T.null plain = ""
  | otherwise = "<p>" <> T.intercalate "<br />" (map escapeHtml (T.lines plain)) <> "</p>"

relationshipJson :: FollowRow -> Value
relationshipJson row =
  object
    [ "id" .= idText otherNumber
    , "following" .= (row.outgoing == Following)
    , "requested" .= (row.outgoing == Requested)
    , "followed_by" .= (row.incoming == Following)
    , "blocking" .= False
    , "blocked_by" .= False
    , "muting" .= False
    , "muting_notifications" .= False
    , "showing_reblogs" .= True
    , "notifying" .= False
    , "domain_blocking" .= False
    , "endorsed" .= False
    , "note" .= ("" :: Text)
    ]
  where
    AccountId otherNumber = row.other

{- | The entities for a page of posts, in the page's order, with their
authors and attachments fetched in two queries for the whole page.
-}
loadStatuses :: Config -> Connection -> [PostRow] -> IO [Value]
loadStatuses config conn rows = do
  authors <- findAccounts conn (unique [row.author | row <- rows])
  media <- mediaOfPosts conn [row.postId | row <- rows]
  pure
    ( mapMaybe
        ( \row -> do
            author <- lookupBy (.accountId) row.author authors
            pure (statusJson config author [m | m <- media, m.post == Just row.postId] row)
        )
        rows
    )
  where
    unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []
    lookupBy key wanted = foldr (\candidate found -> if key candidate == wanted then Just candidate else found) Nothing

-- | What a caller is to a post by the given author (for @can_see@).
viewerOf :: Connection -> Maybe Session -> AccountId -> IO Viewer
viewerOf _ Nothing _ = pure Viewer {isAuthor = False, isAcceptedFollower = False, isModerator = False}
viewerOf conn (Just session) author
  | session.account == author = pure Viewer {isAuthor = True, isAcceptedFollower = False, isModerator = session.isModerator}
  | otherwise = do
      state <- followState conn session.account author
      pure Viewer {isAuthor = False, isAcceptedFollower = state == Following, isModerator = session.isModerator}
