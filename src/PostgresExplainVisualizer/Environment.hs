{-# LANGUAGE RecordWildCards #-}
-- |
module PostgresExplainVisualizer.Environment where

import Data.Pool qualified as P
import Data.Text (Text)
import Data.Word (Word16)
import Database.PostgreSQL.Simple qualified as PG
import Env (
  AsUnread (unread),
  Error,
  Parser,
  Reader,
  help,
  parse,
  str,
  var,
 )
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Servant.Auth.Server (JWTSettings, CookieSettings)

data DeployEnv
  = Test
  | Development
  | Production
  deriving (Eq, Show, Read)

data Config = Config
  { configPort :: !Word16
  , configDatabaseUrl :: !Text
  , configDeployEnv :: !DeployEnv
  , configGithubClientId :: !Text
  , configGithubClientSecret :: !Text
  }
  deriving stock (Eq, Show, Generic)

data AppContext = AppContext
  { ctxPool :: P.Pool PG.Connection
  , ctxPort :: Word16
  , ctxGithubOAuthCredentials :: GithubOAuthCredentials
  , ctxJwtSettings :: JWTSettings
  , ctxCookieSettings :: CookieSettings
  }

data GithubOAuthCredentials = GithubOAuthCredentials
  { clientId :: !Text
  , clientSecret :: !Text
  }

mkAppContext :: P.Pool PG.Connection -> JWTSettings -> CookieSettings -> Config -> AppContext
mkAppContext pool jwtSettings cookieSettings Config{..} =
  AppContext
    { ctxPool = pool
    , ctxPort = configPort
    , ctxGithubOAuthCredentials = GithubOAuthCredentials configGithubClientId configGithubClientSecret
    -- FIXME: I'm sending these around like an idiot because I don't know how to use servant contexts with
    -- servant generic??
    , ctxJwtSettings = jwtSettings
    , ctxCookieSettings = cookieSettings
    }

getServerConfig :: IO Config
getServerConfig = do
  parse id parseConfig

parseConfig :: Parser Error Config
parseConfig =
  Config
    <$> parsePort
    <*> parseDatabaseUrl
    <*> parseDeployEnv
    <*> parseGithubClientId
    <*> parseGithubClientSecret

parseGithubClientSecret :: Parser Error Text
parseGithubClientSecret =
  var str "GH_CLIENT_SECRET" (help "Github OAuth Secret")

parseGithubClientId :: Parser Error Text
parseGithubClientId =
  var str "GH_CLIENT_ID" (help "Github OAuth App ID")

parsePort :: Parser Error Word16
parsePort =
  var port "PORT" (help "HTTP Port for server")

parseDeployEnv :: Parser Error DeployEnv
parseDeployEnv =
  var env "DEPLOY_ENV" (help "Environment to run the server as")

parseDatabaseUrl :: Parser Error Text
parseDatabaseUrl =
  var str "DATABASE_URL" (help "Database URL")

-------------------------------------------------------------------------------
--- PARSER HELPERS
-------------------------------------------------------------------------------

-- | Parse an arbitrary int
int :: Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $ i
  Just parsed -> Right parsed

-- | Parse a valid http port number
port :: Reader Error Word16
port p = case int p of
  Left err -> Left err
  Right intPort ->
    if intPort >= 1 && intPort <= 65535
      then Right $ fromIntegral intPort
      else Left . unread . show $ p

env :: Reader Error DeployEnv
env e = case readMaybe e of
  Nothing -> Left . unread . show $ e
  Just de -> Right de
