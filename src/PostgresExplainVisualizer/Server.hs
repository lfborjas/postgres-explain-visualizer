{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module PostgresExplainVisualizer.Server where
import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant
    ( serveDirectoryWebApp, throwError, Raw, type (:>), ServerError )
import PostgresExplainVisualizer.Types ( AppM )
import Servant.Server.Generic ( genericServeT, AsServerT )
import PostgresExplainVisualizer.Environment
    ( AppContext(..),
      Config(configDatabaseUrl, configDeployEnv, configPort) )
import PostgresExplainVisualizer.Database.Pool qualified as DB
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp
    ( setLogger, setPort, runSettings, defaultSettings )
import Data.Function ((&))
import Data.Pool qualified as P
import Control.Carrier.Reader (runReader)
import PostgresExplainVisualizer.Effects.Database (runDatabaseWithConnection)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import PostgresExplainVisualizer.Server.Pages qualified as Pages

data Routes route = Routes
  { assets :: route :- "static" :> Raw
  , pages  :: route :- Pages.Routes
  }
  deriving stock (Generic)


run :: Config -> IO ()
run config = do
  pool <- DB.initPool (configDatabaseUrl config)
  putStrLn $
    mconcat [
      "[",
      show . configDeployEnv $ config,
      "]",
      "Serving on port ",
      show . configPort $ config
    ]
  let ctx = AppContext pool (configPort config)
  runServer ctx

runServer :: AppContext -> IO ()
runServer ctx = withStdoutLogger $ \logger -> do
  let server = genericServeT (naturalTransform ctx) pevServer
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
            & runM

pevServer :: AppM sig m => Routes (AsServerT m)
pevServer = Routes
  { assets = serveDirectoryWebApp "./static"
  , pages = Pages.server
  }
