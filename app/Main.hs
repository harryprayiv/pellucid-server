{- | The command line: run the server, apply migrations, create an account.

Every setting can come from the environment instead of a flag. The
database URL should: a flag is visible to every user of the machine in
the process list. The password for a new account is read from standard
input and is never an argument.
-}
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import Pellucid.Config (Config (..), Env (..))
import Pellucid.Domain.Limits (defaultLimits)
import Pellucid.Domain.Username (describeUsernameProblem, mkUsername)
import Pellucid.Http.App (application)
import Pellucid.Secret (hashPassword, newSecret)
import Pellucid.Store (Db, closeDb, migrate, openDb, withConnection)
import Pellucid.Store.Accounts (createLocalAccount)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hGetEcho, hIsTerminalDevice, hPutStrLn, hSetEcho, stderr, stdin, stdout)

data Settings = Settings
  { databaseUrl :: String
  , baseUrl :: String
  , title :: String
  , mediaDir :: FilePath
  , host :: String
  , port :: Int
  }

data Command
  = Serve
  | Migrate
  | CreateAccount String Bool

main :: IO ()
main = do
  environment <-
    mapM (\name -> fmap (name,) <$> lookupEnv name) ["PELLUCID_DATABASE_URL", "PELLUCID_BASE_URL", "PELLUCID_TITLE", "PELLUCID_MEDIA_DIR", "PELLUCID_HOST", "PELLUCID_PORT"]
  let fromEnvironment name = lookup name [pair | Just pair <- environment]
  (settings, chosen) <- execParser (info (options fromEnvironment <**> helper) (fullDesc <> progDesc "The Pellucid server"))
  bracket (openDb (TE.encodeUtf8 (T.pack settings.databaseUrl))) closeDb \db -> case chosen of
    Migrate -> runMigrations db
    CreateAccount name moderator -> runMigrations db >> createAccount db name moderator
    Serve -> do
      runMigrations db
      decoy <- hashPassword =<< newSecret
      let
        config =
          Config
            { baseUrl = T.pack settings.baseUrl
            , title = T.pack settings.title
            , databaseUrl = TE.encodeUtf8 (T.pack settings.databaseUrl)
            , mediaDir = settings.mediaDir
            , port = settings.port
            , limits = defaultLimits
            }
        env = Env {config, db, clock = getCurrentTime, decoyPasswordHash = decoy}
        warp =
          Warp.setPort settings.port
            . Warp.setHost (fromString settings.host)
            . Warp.setServerName "pellucid"
            $ Warp.defaultSettings
      hPutStrLn stderr ("pellucid-server: listening on " <> settings.host <> ":" <> show settings.port <> " as " <> settings.baseUrl)
      Warp.runSettings warp (application env)

runMigrations :: Db -> IO ()
runMigrations db = do
  applied <- migrate db
  forM_ applied \name -> hPutStrLn stderr ("pellucid-server: applied migration: " <> T.unpack name)

{- | Rule AccountRegistered, by an operator. The password is one line on
standard input; at a terminal it is asked for without echo.
-}
createAccount :: Db -> String -> Bool -> IO ()
createAccount db name moderator = case mkUsername (T.pack name) of
  Left problem -> failWith (describeUsernameProblem problem)
  Right username -> do
    password <- readPassword
    when (T.length password < 12) (failWith "a password is at least 12 characters")
    hash <- hashPassword password
    created <- withConnection db \conn -> createLocalAccount conn username hash moderator
    case created of
      Just _ -> TIO.putStrLn ("created " <> T.pack name)
      Nothing -> failWith "that username is taken"

readPassword :: IO Text
readPassword = do
  terminal <- hIsTerminalDevice stdin
  if terminal
    then do
      putStr "Password: "
      hFlush stdout
      echo <- hGetEcho stdin
      password <- bracket (hSetEcho stdin False) (\_ -> hSetEcho stdin echo >> putStrLn "") (\_ -> TIO.getLine)
      pure (T.strip password)
    else T.strip <$> TIO.getLine

failWith :: Text -> IO a
failWith why = TIO.hPutStrLn stderr ("pellucid-server: " <> why) >> exitFailure

options :: (String -> Maybe String) -> Parser (Settings, Command)
options fromEnvironment = (,) <$> settings <*> commands
  where
    setting name variable fallback description =
      strOption
        ( long name
            <> metavar "VALUE"
            <> value (fromMaybe fallback (fromEnvironment variable))
            <> showDefaultWith (const fallback)
            <> help (description <> " (environment: " <> variable <> ")")
        )
    settings =
      Settings
        <$> setting "database-url" "PELLUCID_DATABASE_URL" "postgresql:///pellucid" "libpq connection string or URI; prefer the environment variable to this flag"
        <*> setting "base-url" "PELLUCID_BASE_URL" "http://127.0.0.1:8080" "where clients reach this server, scheme and host"
        <*> setting "title" "PELLUCID_TITLE" "Pellucid" "the server's name on its sign-in page"
        <*> setting "media-dir" "PELLUCID_MEDIA_DIR" "media" "directory uploads are stored in"
        <*> setting "host" "PELLUCID_HOST" "127.0.0.1" "address to listen on; the default is this machine only, for a reverse proxy to reach"
        <*> option
          auto
          (long "port" <> metavar "PORT" <> value (fromMaybe 8080 (fromEnvironment "PELLUCID_PORT" >>= readPort)) <> help "port to listen on (environment: PELLUCID_PORT)")
    readPort raw = case reads raw of
      [(n, "")] | n > 0 && n < 65536 -> Just (n :: Int)
      _ -> Nothing
    commands =
      hsubparser
        ( command "serve" (info (pure Serve) (progDesc "Apply migrations, then serve"))
            <> command "migrate" (info (pure Migrate) (progDesc "Apply migrations and stop"))
            <> command
              "create-account"
              ( info
                  (CreateAccount <$> strArgument (metavar "USERNAME") <*> switch (long "moderator" <> help "make the account a moderator"))
                  (progDesc "Create an account; the password is read from standard input")
              )
        )
