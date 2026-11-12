{- | The route table as a value. A route is a method, a path pattern, what
it asks of the caller, how large a body it reads, and a handler. Because
the table is data, the tests can list it and compare it with the routes
the client's spec says it calls.
-}
module Pellucid.Http.Router (
  Route (..),
  Access (..),
  Session (..),
  Request (..),
  route,
  Dispatch (..),
  dispatch,
  pathOf,
  capture,
  requireViewer,
  parse,
) where

import Control.Monad.Trans.Except (throwE)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types (Method)
import Network.Wai qualified as Wai
import Pellucid.Config (Env)
import Pellucid.Domain.Account (AccountStatus)
import Pellucid.Domain.Ids (AccountId)
import Pellucid.Domain.OAuth (Need, Scope)
import Pellucid.Http.Params (BodyLimit, Params, Parser, runParser)
import Pellucid.Http.Respond (Handler, invalid, notFound, unauthorized)

-- | Who a valid token speaks for (actor Member).
data Session = Session
  { account :: AccountId
  , accountStatus :: AccountStatus
  , isModerator :: Bool
  , scopes :: Set Scope
  }

{- | What a route asks of the caller. An open route can be called by
anyone and still learns who is calling when a token is sent.
-}
data Access
  = Open
  | Signed Need

data Request = Request
  { captures :: [Text]
  , params :: Params
  , viewer :: Maybe Session
  , wai :: Wai.Request
  }

data Segment = Literal Text | Variable
  deriving stock (Eq, Show)

data Route = Route
  { method :: Method
  , template :: Text
  , segments :: [Segment]
  , access :: Access
  , body :: BodyLimit
  , handler :: Env -> Request -> Handler Wai.Response
  }

{- | A route from a pattern such as @/api/v1/accounts/:id/follow@, in
which a segment that begins with a colon matches anything.
-}
route :: Method -> Text -> Access -> BodyLimit -> (Env -> Request -> Handler Wai.Response) -> Route
route method template access body handler =
  Route
    { method
    , template
    , segments = [if ":" `T.isPrefixOf` piece then Variable else Literal piece | piece <- pathOf template]
    , access
    , body
    , handler
    }

pathOf :: Text -> [Text]
pathOf = filter (not . T.null) . T.splitOn "/"

data Dispatch
  = Found Route [Text]
  | -- | The path exists under other methods, which are listed.
    WrongMethod [Method]
  | NoRoute

{- | The first route in the table whose pattern matches the path and whose
method is the request's. The table lists literal paths before patterns
that would also match them.
-}
dispatch :: [Route] -> Method -> [Text] -> Dispatch
dispatch table method path =
  case [(candidate, found) | candidate <- table, Just found <- [match candidate.segments path]] of
    [] -> NoRoute
    matching -> case [hit | hit@(candidate, _) <- matching, candidate.method == method] of
      (candidate, found) : _ -> Found candidate found
      [] -> WrongMethod [candidate.method | (candidate, _) <- matching]

match :: [Segment] -> [Text] -> Maybe [Text]
match [] [] = Just []
match (Literal expected : rest) (piece : pieces)
  | expected == piece = match rest pieces
match (Variable : rest) (piece : pieces) = (piece :) <$> match rest pieces
match _ _ = Nothing

{- | The nth captured segment. The pattern fixes how many there are, so a
miss is a mistake in the table and is answered as a missing page.
-}
capture :: Int -> Request -> Handler Text
capture n request = case drop n request.captures of
  value : _ -> pure value
  [] -> throwE notFound

requireViewer :: Request -> Handler Session
requireViewer request = maybe (throwE (unauthorized "this route needs a token")) pure request.viewer

{- | Read what a route needs from the request, or refuse with every field
that is wrong.
-}
parse :: Request -> Parser a -> Handler a
parse request parser = either (throwE . invalid) pure (runParser parser request.params)
