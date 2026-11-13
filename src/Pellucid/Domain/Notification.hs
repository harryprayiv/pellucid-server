{- | Notifications: what kinds there are, what each is called in the client
API, and the one step of their lifecycle.
-}
module Pellucid.Domain.Notification (
  NotificationKind (..),
  kindText,
  parseKind,
  kindToApi,
  NotificationStatus (..),
  notificationStatusText,
  parseNotificationStatus,
  notificationNext,
) where

import Data.Text (Text)

{- | The spec's enum NotificationKind. Four are produced so far: a follow,
a follow request, a like and a comment.
-}
data NotificationKind
  = Followed
  | FollowRequested
  | Mentioned
  | Shared
  | Liked
  | Commented
  | Tagged
  | MessageReceived
  | ModerationNotice
  deriving stock (Eq, Show, Enum, Bounded)

-- | The name the store and the spec use.
kindText :: NotificationKind -> Text
kindText = \case
  Followed -> "followed"
  FollowRequested -> "follow_requested"
  Mentioned -> "mentioned"
  Shared -> "shared"
  Liked -> "liked"
  Commented -> "commented"
  Tagged -> "tagged"
  MessageReceived -> "message_received"
  ModerationNotice -> "moderation_notice"

parseKind :: Text -> Maybe NotificationKind
parseKind = \case
  "followed" -> Just Followed
  "follow_requested" -> Just FollowRequested
  "mentioned" -> Just Mentioned
  "shared" -> Just Shared
  "liked" -> Just Liked
  "commented" -> Just Commented
  "tagged" -> Just Tagged
  "message_received" -> Just MessageReceived
  "moderation_notice" -> Just ModerationNotice
  _ -> Nothing

{- | The @type@ a client of this API reads. These are the words Pixelfed
sends for the same events, which are not always the words Mastodon
uses: a like is @favourite@, a share is @share@ and a comment is
@comment@.
-}
kindToApi :: NotificationKind -> Text
kindToApi = \case
  Followed -> "follow"
  FollowRequested -> "follow_request"
  Mentioned -> "mention"
  Shared -> "share"
  Liked -> "favourite"
  Commented -> "comment"
  Tagged -> "tagged"
  MessageReceived -> "direct"
  ModerationNotice -> "modlog"

data NotificationStatus = Unread | Seen
  deriving stock (Eq, Show, Enum, Bounded)

notificationStatusText :: NotificationStatus -> Text
notificationStatusText = \case
  Unread -> "unread"
  Seen -> "seen"

parseNotificationStatus :: Text -> Maybe NotificationStatus
parseNotificationStatus = \case
  "unread" -> Just Unread
  "seen" -> Just Seen
  _ -> Nothing

-- | Entity Notification's transition graph. 'Seen' is terminal.
notificationNext :: NotificationStatus -> [NotificationStatus]
notificationNext = \case
  Unread -> [Seen]
  Seen -> []
