{- | A PostgreSQL server that exists for one test run: a fresh cluster in a
temporary directory, reachable through a socket in that directory only,
stopped and deleted afterwards. Nothing is assumed to be running and
nothing outside the directory is touched.

@initdb@ and @pg_ctl@ must be on the PATH, which the dev shell sees to.
PostgreSQL refuses to run as root, so the tests cannot either.
-}
module Test.TempPostgres (
  withTempPostgres,
) where

import Control.Exception (finally)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

-- | Run an action with the connection string of a throwaway database.
withTempPostgres :: (ByteString -> IO a) -> IO a
withTempPostgres use = withSystemTempDirectory "pellucid-pg" \dir -> do
  let
    cluster = dir </> "data"
    settings = "-c listen_addresses='' -c unix_socket_directories='" <> dir <> "' -c fsync=off"
  run "initdb" ["-D", cluster, "-A", "trust", "-U", "postgres", "--no-sync", "-E", "UTF8", "--locale=C"]
  run "pg_ctl" ["-D", cluster, "-w", "-o", settings, "-l", dir </> "postgres.log", "start"]
  use (BC.pack ("host=" <> dir <> " dbname=postgres user=postgres"))
    `finally` run "pg_ctl" ["-D", cluster, "-m", "immediate", "-w", "stop"]

run :: FilePath -> [String] -> IO ()
run program arguments = do
  (code, out, err) <- readProcessWithExitCode program arguments ""
  unless (code == ExitSuccess) $
    ioError (userError (program <> " failed: " <> out <> err))
