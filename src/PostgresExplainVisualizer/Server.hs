{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module PostgresExplainVisualizer.Server where

import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import Control.Carrier.Reader (runReader)
import Data.Function ((&))
import Data.Pool qualified as P
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setLogger,
  setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import PostgresExplainVisualizer.Database.Pool qualified as DB
import PostgresExplainVisualizer.Effects.Database (runDatabaseWithConnection)
import PostgresExplainVisualizer.Environment (
  AppContext (..),
  Config (configDatabaseUrl, configDeployEnv, configPort), mkAppContext
 )
import PostgresExplainVisualizer.Server.Pages qualified as Pages
import PostgresExplainVisualizer.Types (AppM)
import Servant (
  Raw,
  ServerError,
  serveDirectoryWebApp,
  throwError,
  type (:>), Context ((:.), EmptyContext)
 )
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.Server.Generic (AsServerT, genericServeTWithContext)
import PostgresExplainVisualizer.Effects.Http (HttpClient(runHttp))
import PostgresExplainVisualizer.Effects.Log (LogStdoutC(runLogStdout))
import Servant.Auth.Server (readKey, defaultCookieSettings)
import Servant.Auth.Server.Internal.ConfigTypes (defaultJWTSettings)
import PostgresExplainVisualizer.Effects.Crypto (CryptoIOC(runCryptoIO))

data Routes route = Routes
  { assets :: route :- "static" :> Raw
  , pages :: route :- Pages.Routes
  }
  deriving stock (Generic)

run :: Config -> IO ()
run config = do
  pool <- DB.initPool (configDatabaseUrl config)
  jwtKey <- readKey "config/JWT.key"
  let jwtSettings = defaultJWTSettings jwtKey
      cookieSettings = defaultCookieSettings
  putStrLn $
    mconcat
      [ "["
      , show . configDeployEnv $ config
      , "]"
      , "Serving on port "
      , show . configPort $ config
      ]
  let ctx = mkAppContext pool jwtSettings cookieSettings config
  runServer ctx

runServer :: AppContext -> IO ()
runServer ctx = withStdoutLogger $ \logger -> do
  let serverCfg = ctxCookieSettings ctx :. ctxJwtSettings ctx :. EmptyContext
      server = genericServeTWithContext (naturalTransform ctx) pevServer serverCfg
      warpSettings =
        defaultSettings
          & setPort (fromIntegral $ ctxPort ctx)
          & setLogger logger
  runSettings warpSettings server
 where
  naturalTransform cfg handler = do
    response <- P.withResource (ctxPool ctx) runEffects
    either throwError pure response
   where
    runEffects conn =
      handler
        & runError @ServerError
        & runReader cfg
        & runDatabaseWithConnection conn
        & runHttp
        & runLogStdout
        & runCryptoIO
        & runM

pevServer :: AppM sig m => Routes (AsServerT m)
pevServer =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , pages = Pages.server
    }
