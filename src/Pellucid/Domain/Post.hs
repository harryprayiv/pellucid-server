{- | Posts and their media: lifecycles, who may see a post, and what rule
PostPublished asks of a submission.
-}
module Pellucid.Domain.Post (
  Visibility (..),
  visibilityText,
  parseVisibility,
  visibilityFromApi,
  visibilityToApi,
  PostStatus (..),
  postStatusText,
  parsePostStatus,
  postNext,
  MediaStatus (..),
  mediaStatusText,
  parseMediaStatus,
  mediaNext,
  Viewer (..),
  stranger,
  canSee,
  canSeeComment,
  Attachment (..),
  Submission (..),
  PublishProblem (..),
  checkPublish,
  describePublishProblem,
  mayWithdraw,
  mayTakeBack,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Pellucid.Domain.Account (AccountStatus, canAct)
import Pellucid.Domain.Limits (Limits (..))

data Visibility = Public | Unlisted | FollowersOnly | Direct
  deriving stock (Eq, Show, Enum, Bounded)

-- | The name the store and the spec use.
visibilityText :: Visibility -> Text
visibilityText = \case
  Public -> "public"
  Unlisted -> "unlisted"
  FollowersOnly -> "followers_only"
  Direct -> "direct"

parseVisibility :: Text -> Maybe Visibility
parseVisibility = \case
  "public" -> Just Public
  "unlisted" -> Just Unlisted
  "followers_only" -> Just FollowersOnly
  "direct" -> Just Direct
  _ -> Nothing

-- | The client API's name for followers-only is @private@.
visibilityFromApi :: Text -> Maybe Visibility
visibilityFromApi = \case
  "private" -> Just FollowersOnly
  other -> parseVisibility other

visibilityToApi :: Visibility -> Text
visibilityToApi = \case
  FollowersOnly -> "private"
  other -> visibilityText other

data PostStatus = Published | Withdrawn | TakenDown
  deriving stock (Eq, Show, Enum, Bounded)

postStatusText :: PostStatus -> Text
postStatusText = \case
  Published -> "published"
  Withdrawn -> "withdrawn"
  TakenDown -> "taken_down"

parsePostStatus :: Text -> Maybe PostStatus
parsePostStatus = \case
  "published" -> Just Published
  "withdrawn" -> Just Withdrawn
  "taken_down" -> Just TakenDown
  _ -> Nothing

-- | Entity Post's transition graph. 'Withdrawn' is terminal.
postNext :: PostStatus -> [PostStatus]
postNext = \case
  Published -> [Withdrawn, TakenDown]
  TakenDown -> [Published, Withdrawn]
  Withdrawn -> []

data MediaStatus = Received | Processing | Ready | Attached | Unusable | Purged
  deriving stock (Eq, Show, Enum, Bounded)

mediaStatusText :: MediaStatus -> Text
mediaStatusText = \case
  Received -> "received"
  Processing -> "processing"
  Ready -> "ready"
  Attached -> "attached"
  Unusable -> "unusable"
  Purged -> "purged"

parseMediaStatus :: Text -> Maybe MediaStatus
parseMediaStatus = \case
  "received" -> Just Received
  "processing" -> Just Processing
  "ready" -> Just Ready
  "attached" -> Just Attached
  "unusable" -> Just Unusable
  "purged" -> Just Purged
  _ -> Nothing

-- | Entity Media's transition graph. 'Purged' is terminal.
mediaNext :: MediaStatus -> [MediaStatus]
mediaNext = \case
  Received -> [Processing]
  Processing -> [Ready, Unusable]
  Ready -> [Attached, Purged]
  Unusable -> [Purged]
  Attached -> [Purged]
  Purged -> []

-- | What the reader of a post is to it.
data Viewer = Viewer
  { isAuthor :: Bool
  , isAcceptedFollower :: Bool
  , isModerator :: Bool
  }
  deriving stock (Eq, Show)

{- | Someone with no tie to the post: not signed in, or signed in and
neither its author, a follower of the author, nor a moderator.
-}
stranger :: Viewer
stranger = Viewer {isAuthor = False, isAcceptedFollower = False, isModerator = False}

{- | The spec's reading rule @can_see@, without the part about blocks,
which arrive with rule AccountBlocked. A direct post is seen by its
author only until direct messages are built.
-}
canSee :: PostStatus -> Visibility -> Viewer -> Bool
canSee status visibility viewer = case status of
  Withdrawn -> False
  TakenDown -> viewer.isAuthor || viewer.isModerator
  Published -> case visibility of
    Public -> True
    Unlisted -> True
    FollowersOnly -> viewer.isAuthor || viewer.isAcceptedFollower
    Direct -> viewer.isAuthor

{- | The reading rule for a comment. It is read by whoever can read the
post it is under, which the caller has worked out with 'canSee' for the
post at the top of the thread, and by its own author. The viewer here is
what the reader is to the comment's author. A comment a moderator has
taken down is read as a taken-down post is.
-}
canSeeComment :: PostStatus -> Viewer -> Bool -> Bool
canSeeComment status viewer threadReadable = case status of
  Withdrawn -> False
  TakenDown -> viewer.isAuthor || viewer.isModerator
  Published -> viewer.isAuthor || threadReadable

-- | One of the uploads a submission names, as the store finds it.
data Attachment = Attachment
  { ownedByAuthor :: Bool
  , status :: MediaStatus
  }
  deriving stock (Eq, Show)

data Submission = Submission
  { caption :: Text
  , spoilerText :: Text
  , visibility :: Visibility
  , attachments :: [Attachment]
  }
  deriving stock (Eq, Show)

data PublishProblem
  = AuthorNotActive
  | NoAttachment
  | TooManyAttachments Int
  | AttachmentNotUsable
  | CaptionTooLong Int
  | SpoilerTooLong Int
  | DirectIsAMessage
  deriving stock (Eq, Show)

{- | Rule PostPublished: every reason the submission is refused, or none.
Lengths are counted in characters, the unit the limits are published in.
-}
checkPublish :: Limits -> AccountStatus -> Submission -> [PublishProblem]
checkPublish limits authorStatus submission =
  concat
    [ [AuthorNotActive | not (canAct authorStatus)]
    , [NoAttachment | count == 0]
    , [TooManyAttachments limits.maxAlbumLength | count > limits.maxAlbumLength]
    , [AttachmentNotUsable | not (all usable submission.attachments)]
    , [CaptionTooLong limits.maxCaptionLength | T.length submission.caption > limits.maxCaptionLength]
    , [SpoilerTooLong limits.maxSpoilerLength | T.length submission.spoilerText > limits.maxSpoilerLength]
    , [DirectIsAMessage | submission.visibility == Direct]
    ]
  where
    count = length submission.attachments
    usable attachment = attachment.ownedByAuthor && attachment.status == Ready

describePublishProblem :: PublishProblem -> (Text, Text)
describePublishProblem = \case
  AuthorNotActive -> ("account", "this account cannot post while it is suspended or leaving")
  NoAttachment -> ("media_ids", "a post needs at least one photo")
  TooManyAttachments limit -> ("media_ids", "a post holds at most " <> T.pack (show limit) <> " attachments")
  AttachmentNotUsable -> ("media_ids", "every attachment must be an upload of yours that is not yet on a post")
  CaptionTooLong limit -> ("status", "a caption is at most " <> T.pack (show limit) <> " characters")
  SpoilerTooLong limit -> ("spoiler_text", "a content warning is at most " <> T.pack (show limit) <> " characters")
  DirectIsAMessage -> ("visibility", "a direct post is a direct message; direct messages are not built yet")

{- | Rule PostWithdrawn: the author can always delete, including a post a
moderator has taken down.
-}
mayWithdraw :: Bool -> PostStatus -> Bool
mayWithdraw isAuthor status = isAuthor && Withdrawn `elem` postNext status

{- | Rule MediaTakenBack (S13): an upload can be deleted by its owner
until it is attached to a post.
-}
mayTakeBack :: Bool -> MediaStatus -> Bool
mayTakeBack isOwner status = isOwner && (status == Ready || status == Unusable)
