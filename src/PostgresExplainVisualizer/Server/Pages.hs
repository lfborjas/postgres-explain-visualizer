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
      Get, Capture )
import Servant.API.Generic
    ( Generic, GenericMode(type (:-)), ToServant, ToServantApi )
import Servant.HTML.Lucid ( HTML )
import Servant.Server.Generic ( genericServerT, AsServerT )
import PostgresExplainVisualizer.Models.Plan
import PostgresExplainVisualizer.Types (AppM)
import PostgresExplainVisualizer.Effects.Database (select)
import Data.Maybe (listToMaybe)
import Servant.Server
import Control.Carrier.Error.Either (throwError)
import PostgresExplainVisualizer.Models.Common
import qualified PostgresExplainVisualizer.Views.Layout as Layout
import qualified PostgresExplainVisualizer.Views.PEV2 as PEV2

type Routes = ToServantApi Routes'
type Param' = QueryParam' '[Required, Lenient]

data Routes' mode = Routes'
  { showPlan ::
      mode :- "plan"
      :> Capture "planId" PlanID
      :> Get '[HTML] (Html ())
  } deriving stock (Generic)

server :: AppM sig m => ToServant Routes' (AsServerT m)
server = genericServerT Routes'
  { showPlan = showPlanHandler
  }

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
