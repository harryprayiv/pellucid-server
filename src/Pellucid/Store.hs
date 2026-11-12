-- | The connection pool, transactions, and the migration runner.
module Pellucid.Store (
  Db,
  openDb,
  closeDb,
  withConnection,
  transact,
  migrate,
  parsed,
) where

import Control.Exception (mask, onException)
import Control.Monad (forM, void)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, setNumStripes, withResource)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (
  Connection,
  Only (..),
  begin,
  close,
  commit,
  connectPostgreSQL,
  execute,
  execute_,
  query_,
  rollback,
 )
import Database.PostgreSQL.Simple.FromField (ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (RowParser, fieldWith)
import Pellucid.Store.Schema (migrations)

type Db = Pool Connection

{- | A pool of at most eight connections, each closed after thirty idle
seconds. The URL is a libpq connection string or URI. The pool is one
stripe: by default it would make one per processor, and it refuses to
start with more stripes than connections.
-}
openDb :: ByteString -> IO Db
openDb url = newPool (setNumStripes (Just 1) (defaultPoolConfig (connectPostgreSQL url) close 30 8))

closeDb :: Db -> IO ()
closeDb = destroyAllResources

withConnection :: Db -> (Connection -> IO a) -> IO a
withConnection = withResource

{- | Run an action in one transaction. A 'Left' rolls it back, as does an
exception, so a rule that is refused halfway leaves nothing behind.
-}
transact :: Connection -> IO (Either e a) -> IO (Either e a)
transact conn action = mask \restore -> do
  begin conn
  result <- restore action `onException` rollback conn
  case result of
    Left _ -> rollback conn
    Right _ -> commit conn
  pure result

{- | Apply every migration that has not been applied, in order, inside one
transaction and under a lock, so that two servers starting together do
not both try. Returns the names of the ones applied now.
-}
migrate :: Db -> IO [Text]
migrate db = withConnection db \conn -> do
  result <- transact conn do
    -- Without this the server says, on every start after the first, that
    -- the table already exists.
    void (execute_ conn "SET LOCAL client_min_messages = warning")
    void (execute_ conn "CREATE TABLE IF NOT EXISTS schema_migrations (version integer PRIMARY KEY, name text NOT NULL, applied_at timestamptz NOT NULL DEFAULT now())")
    _ <- query_ conn "SELECT pg_advisory_xact_lock(7355608)" :: IO [Only ()]
    done <- map fromOnly <$> query_ conn "SELECT version FROM schema_migrations" :: IO [Int]
    applied <- forM [m | m@(version, _, _) <- sortOn (\(version, _, _) -> version) migrations, version `notElem` done] \(version, name, statements) -> do
      void (execute_ conn statements)
      void (execute conn "INSERT INTO schema_migrations (version, name) VALUES (?, ?)" (version, name))
      pure name
    pure (Right applied :: Either () [Text])
  pure (either (const []) id result)

{- | Read a text column into a type of the domain. A value the code does
not know is a conversion error, not a default: the database's CHECK
constraints and the parsers are meant to agree, and a disagreement
should be loud.
-}
parsed :: (Typeable a) => (Text -> Maybe a) -> RowParser a
parsed parse = fieldWith \f bytes -> do
  text <- fromField f bytes
  maybe (returnError ConversionFailed f "a value this version of the server does not know") pure (parse text)
