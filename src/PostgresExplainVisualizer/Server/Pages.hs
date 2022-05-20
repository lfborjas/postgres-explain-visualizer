{-# LANGUAGE ImportQualifiedPost #-}
-- |

module PostgresExplainVisualizer.Server.Pages where

import Lucid ( Html )
import Servant
    ( Lenient,
      Required,
      QueryFlag,
      QueryParam',
      QueryParams,
      type (:>),
      Get, Capture, QueryParam, Post, Optional )
import Servant.API.Generic
    ( Generic, GenericMode(type (:-)), ToServant, ToServantApi )
import Servant.HTML.Lucid ( HTML )
import Servant.Server.Generic ( genericServerT, AsServerT )
import PostgresExplainVisualizer.Models.Plan
import PostgresExplainVisualizer.Types (AppM)
import PostgresExplainVisualizer.Effects.Database (select, insert)
import Data.Maybe (listToMaybe, catMaybes)
import Servant.Server
import Control.Carrier.Error.Either (throwError)
import PostgresExplainVisualizer.Models.Common
import qualified PostgresExplainVisualizer.Views.Layout as Layout
import qualified PostgresExplainVisualizer.Views.PEV2 as PEV2
import qualified PostgresExplainVisualizer.Views.NewPlan as NewPlan
import Data.Text ( intercalate, Text, pack )
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)

type Routes = ToServantApi Routes'
type RequiredParam' = QueryParam' '[Required, Lenient]
type OptionalParam' = QueryParam' '[Optional, Lenient]

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ())
  , createPlan ::
      mode :- "plans"
      :> RequiredParam' "plan" NonEmptyText
      :> OptionalParam' "query" NonEmptyText
      :> Post '[HTML] (Html ())
  , showPlan ::
      mode :- "plan"
      :> Capture "planId" PlanID
      :> Get '[HTML] (Html ())
  } deriving stock (Generic)

server :: AppM sig m => ToServant Routes' (AsServerT m)
server = genericServerT Routes'
  { home = homeHandler
  , createPlan = createPlanHandler
  , showPlan = showPlanHandler
  }

homeHandler
  :: AppM sig m
  => m (Html ())
homeHandler = do
  renderView $ NewPlan.page Nothing

createPlanHandler
  :: AppM sig m
  => Either Text NonEmptyText
  -> Maybe (Either Text NonEmptyText)
  -> m (Html ())
createPlanHandler parsedPlan parsedQuery = do
  -- FIXME: surely there's a better way to do this in a transformer, I'm just out of time here!
  case parsedPlan of
    Left e -> renderView . NewPlan.page $ Just e
    Right p -> do
      let q =
            case parsedQuery of
              Nothing -> Nothing
              Just (Left _e) -> Nothing
              Just (Right q') -> Just q'
      createdPlan <- insert $ newPlan p q
      case listToMaybe createdPlan of
        Nothing -> throwError $ err500 {errBody = "Unable to store plan, sorry!"}
        Just (createdId, _, _) ->
          redirect $ "/plan/" <> (pack . show $ createdId)

showPlanHandler
  :: AppM sig m
  => PlanID
  -> m (Html ())
showPlanHandler pid = do
  plan <- select $ planByID pid
  case listToMaybe plan of
    Nothing -> throwError $ err404 {errBody = "Plan doesn't exist"}
    Just (_, pSource, pQuery, createdAt) ->
      renderView $ PEV2.page pSource pQuery createdAt


renderView :: AppM sig m => Html () -> m (Html ())
renderView = pure . Layout.layout

redirect :: AppM sig m => Text -> m (Html ())
redirect loc = do
  throwError $ err301 {errHeaders = [("Location", encodeUtf8 loc)] }
