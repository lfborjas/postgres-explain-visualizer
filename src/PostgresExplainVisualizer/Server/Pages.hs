module PostgresExplainVisualizer.Server.Pages where

import Control.Carrier.Error.Either (throwError)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Lucid (Html)
import PostgresExplainVisualizer.Effects.Database (insert, select)
import PostgresExplainVisualizer.Models.Plan
    ( NonEmptyText, PlanID, newPlan, planByID )
import PostgresExplainVisualizer.Types (AppM)
import PostgresExplainVisualizer.Views.Layout qualified as Layout
import PostgresExplainVisualizer.Views.NewPlan qualified as NewPlan
import PostgresExplainVisualizer.Views.PEV2 qualified as PEV2
import Servant (
  Capture,
  FormUrlEncoded,
  Get,
  Post,
  ReqBody,
  type (:>),
 )
import Servant.API.Generic (
  Generic,
  GenericMode (type (:-)),
  ToServant,
  ToServantApi,
 )
import Servant.HTML.Lucid (HTML)
import Servant.Server
    ( ServerError(errBody, errHeaders), err301, err404, err500 )
import Servant.Server.Generic (AsServerT, genericServerT)
import Web.Internal.FormUrlEncoded (FromForm (..), parseUnique)

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ())
  , createPlan ::
      mode :- "plans"
        :> ReqBody '[FormUrlEncoded] PlanRequest
        :> Post '[HTML] (Html ())
  , showPlan ::
      mode :- "plan"
        :> Capture "planId" PlanID
        :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

data PlanRequest = PlanRequest
  { planParam :: NonEmptyText
  , queryParam :: Maybe NonEmptyText
  }

-- cf:
-- https://stackoverflow.com/a/39819730
-- https://hackage.haskell.org/package/http-api-data-0.4.1.1/docs/Web-Internal-FormUrlEncoded.html#t:FromForm
instance FromForm PlanRequest where
  fromForm f =
    let parsedPlan = parseUnique "plan" f
        parsedQuery =
          -- NOTE: showing a bit of leniency to browser which send this
          -- as an empty string.
          case parseUnique "query" f of
            Left _ -> Right Nothing
            Right e -> Right $ Just e
     in PlanRequest <$> parsedPlan <*> parsedQuery

server :: AppM sig m => ToServant Routes' (AsServerT m)
server =
  genericServerT
    Routes'
      { home = homeHandler
      , createPlan = createPlanHandler
      , showPlan = showPlanHandler
      }

homeHandler ::
  AppM sig m =>
  m (Html ())
homeHandler = do
  renderView $ NewPlan.page Nothing

createPlanHandler ::
  AppM sig m =>
  PlanRequest ->
  m (Html ())
createPlanHandler PlanRequest{planParam, queryParam} = do
  createdPlan <- insert $ newPlan planParam queryParam
  case listToMaybe createdPlan of
    Nothing -> throwError $ err500{errBody = "Unable to store plan, sorry!"}
    Just (createdId, _, _) ->
      redirect $ "/plan/" <> (pack . show $ createdId)

showPlanHandler ::
  AppM sig m =>
  PlanID ->
  m (Html ())
showPlanHandler pid = do
  plan <- select $ planByID pid
  case listToMaybe plan of
    Nothing -> throwError $ err404{errBody = "Plan doesn't exist"}
    Just (_, pSource, pQuery, createdAt) ->
      renderView $ PEV2.page pSource pQuery createdAt

renderView :: AppM sig m => Html () -> m (Html ())
renderView = pure . Layout.layout

redirect :: AppM sig m => Text -> m (Html ())
redirect loc = do
  throwError $ err301{errHeaders = [("Location", encodeUtf8 loc)]}
