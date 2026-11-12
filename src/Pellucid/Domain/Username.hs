{- | Usernames. A username is kept as its owner wrote it and compared
without case (rule AccountRegistered).
-}
module Pellucid.Domain.Username (
  Username,
  UsernameProblem (..),
  mkUsername,
  usernameText,
  usernameKey,
  describeUsernameProblem,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as T

-- | Only 'mkUsername' makes one, so a value of this type is always valid.
newtype Username = Username Text
  deriving stock (Eq, Show)

data UsernameProblem
  = UsernameEmpty
  | UsernameTooLong
  | UsernameBadCharacter Char
  | UsernameBadEdge
  deriving stock (Eq, Show)

{- | One to thirty characters from the ASCII letters, the digits and @_@,
with @.@ and @-@ allowed inside but not at either end. The set is what a
handle can carry unescaped in a URL, a WebFinger address and a mention.
-}
mkUsername :: Text -> Either UsernameProblem Username
mkUsername raw
  | T.null name = Left UsernameEmpty
  | T.length name > 30 = Left UsernameTooLong
  | Just bad <- T.find (not . allowed) name = Left (UsernameBadCharacter bad)
  | edge (T.take 1 name) || edge (T.takeEnd 1 name) = Left UsernameBadEdge
  | otherwise = Right (Username name)
  where
    name = T.strip raw
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '.' || c == '-'
    edge c = c == "." || c == "-"

usernameText :: Username -> Text
usernameText (Username name) = name

-- | The form two usernames are compared in.
usernameKey :: Username -> Text
usernameKey (Username name) = T.toLower name

describeUsernameProblem :: UsernameProblem -> Text
describeUsernameProblem = \case
  UsernameEmpty -> "a username cannot be empty"
  UsernameTooLong -> "a username is at most 30 characters"
  UsernameBadCharacter c -> "a username cannot contain " <> T.singleton c
  UsernameBadEdge -> "a username cannot begin or end with . or -"
