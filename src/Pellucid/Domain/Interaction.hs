{- | What an account does to a post: like it, bookmark it, comment on it.
Each decision is named for the rule of the spec it carries out.
-}
module Pellucid.Domain.Interaction (
  Mark (..),
  MarkRefusal (..),
  MarkOutcome (..),
  decideLike,
  decideBookmark,
  recipientOf,
  CommentSubmission (..),
  CommentProblem (..),
  checkComment,
  describeCommentProblem,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Pellucid.Domain.Account (AccountStatus, canAct)
import Pellucid.Domain.Ids (AccountId)
import Pellucid.Domain.Limits (Limits (..))

-- | What stands when an account asks to like or to bookmark a post.
data Mark = Mark
  { actorStatus :: AccountStatus
  , readable :: Bool
  -- ^ The post is published and the account can see it (@can_see@).
  , standing :: Bool
  -- ^ The like or the bookmark is already there.
  }
  deriving stock (Eq, Show)

data MarkRefusal
  = -- | Answered as a missing page, the same as a post that does not exist.
    PostOutOfReach
  | ActorNotActive
  deriving stock (Eq, Show)

data MarkOutcome
  = Place
  | -- | Nothing changes, and the answer is the post as it is. Asking twice
    -- is not an error in this API.
    AlreadyPlaced
  deriving stock (Eq, Show)

{- | Rule PostLiked. Whether the post can be read is asked first, so that a
suspended account learns nothing about a post it could not have seen.
-}
decideLike :: Mark -> Either MarkRefusal MarkOutcome
decideLike mark
  | not mark.readable = Left PostOutOfReach
  | not (canAct mark.actorStatus) = Left ActorNotActive
  | mark.standing = Right AlreadyPlaced
  | otherwise = Right Place

{- | Rule PostBookmarked. A bookmark is private: it tells nobody and
changes nothing anyone else reads, so the rule asks nothing of the
account's standing.
-}
decideBookmark :: Mark -> Either MarkRefusal MarkOutcome
decideBookmark mark
  | not mark.readable = Left PostOutOfReach
  | mark.standing = Right AlreadyPlaced
  | otherwise = Right Place

{- | Who is told of a like or a comment: the author of the post, and nobody
when the author is the one who did it.
-}
recipientOf :: AccountId -> AccountId -> Maybe AccountId
recipientOf actor author
  | actor == author = Nothing
  | otherwise = Just author

data CommentSubmission = CommentSubmission
  { authorStatus :: AccountStatus
  , parentReadable :: Bool
  -- ^ The parent is published and the commenter can see it.
  , commentsDisabled :: Bool
  , text :: Text
  }
  deriving stock (Eq, Show)

data CommentProblem
  = ParentOutOfReach
  | CommenterNotActive
  | CommentsAreOff
  | EmptyComment
  | CommentTooLong Int
  deriving stock (Eq, Show)

{- | Rule CommentPublished: every reason the comment is refused, or none.
A parent that cannot be read is the only reason given when it applies,
because the others would say something about a post the commenter may
not see. Blocks are not consulted until rule AccountBlocked is built.
-}
checkComment :: Limits -> CommentSubmission -> [CommentProblem]
checkComment limits submission
  | not submission.parentReadable = [ParentOutOfReach]
  | otherwise =
      concat
        [ [CommenterNotActive | not (canAct submission.authorStatus)]
        , [CommentsAreOff | submission.commentsDisabled]
        , [EmptyComment | T.null (T.strip submission.text)]
        , [CommentTooLong limits.maxCaptionLength | T.length submission.text > limits.maxCaptionLength]
        ]

describeCommentProblem :: CommentProblem -> (Text, Text)
describeCommentProblem = \case
  ParentOutOfReach -> ("in_reply_to_id", "names no post you can read")
  CommenterNotActive -> ("account", "this account cannot comment while it is suspended or leaving")
  CommentsAreOff -> ("in_reply_to_id", "comments are turned off on this post")
  EmptyComment -> ("status", "a comment needs some text")
  CommentTooLong limit -> ("status", "a comment is at most " <> T.pack (show limit) <> " characters")
