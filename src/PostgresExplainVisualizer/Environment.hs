-- |

module PostgresExplainVisualizer.Environment where

import Data.Word (Word16)
import Env
  ( AsUnread (unread),
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
import Data.Text ( Text )
import Data.Pool qualified as P
import Database.PostgreSQL.Simple qualified as PG

data DeployEnv
  = Test
  | Development
  | Production
  deriving (Eq, Show, Read)

data Config = Config
  { configPort :: !Word16
  , configDatabaseUrl :: !Text
  , configDeployEnv :: !DeployEnv
  }
  deriving stock (Eq, Show, Generic)

data AppContext = AppContext
  { ctxPool :: P.Pool PG.Connection
  , ctxPort :: Word16
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
  Right intPort -> if intPort >= 1 && intPort <= 65535
    then Right $ fromIntegral intPort
    else Left . unread . show $ p

env :: Reader Error DeployEnv
env e = case readMaybe e of
  Nothing -> Left . unread . show $ e
  Just de -> Right de
