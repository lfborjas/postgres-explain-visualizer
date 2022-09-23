{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module PostgresExplainVisualizer.Server.Pages where

import Control.Carrier.Error.Either (throwError)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Lucid (Html, renderBS)
import PostgresExplainVisualizer.Effects.Database (insert, select)
import PostgresExplainVisualizer.Models.Plan (
  NonEmptyText,
  PlanID,
  newPlan,
  planByID,
 )
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
  type (:>), Verb, StdMethod (GET), Headers, Header, NoContent (NoContent), QueryParam, QueryParam', Required, Strict, addHeader, err401
 )
import Servant.API.Generic (
  Generic,
  GenericMode (type (:-)),
  ToServant,
  ToServantApi
 )
import Servant.HTML.Lucid (HTML)
import Servant.Server (
  ServerError (errBody, errHeaders),
  err301,
  err404,
  err500,
 )
import Servant.Server.Generic (AsServerT, genericServerT)
import Web.Internal.FormUrlEncoded (FromForm (..), parseUnique)
import Data.Kind (Type)
import PostgresExplainVisualizer.Client.Github.Api
import PostgresExplainVisualizer.Environment (AppContext(..))
import qualified Data.UUID as UUID
import Control.Carrier.Reader (ask)
import PostgresExplainVisualizer.Effects.Http
import PostgresExplainVisualizer.Effects.Log
import Prelude hiding (log)
import Servant.Auth.Server ( ToJWT, FromJWT, Auth, SetCookie )
import Data.Aeson (ToJSON, FromJSON)
import Servant.Auth.Server qualified as Auth
import PostgresExplainVisualizer.Effects.Crypto qualified as Crypto
import Web.Cookie
import Data.ByteString (ByteString)


type Routes = ToServantApi Routes'
type Get302 (cts :: [Type]) (hs :: [Type]) a = Verb 'GET 302 cts (Headers (Header "Location" Text ': hs) a)
type StrictParam = QueryParam' '[Required, Strict]

data Routes' mode = Routes'
  { routesWithSession :: mode :- Auth '[Auth.Cookie] Session :> ToServantApi RoutesWithSession
  , routesWithoutSession :: mode :- ToServantApi RoutesWithoutSession
  }
  deriving stock (Generic)

-- | Routes that may use a session cookie when present
data RoutesWithSession mode = RoutesWithSession
  { home :: mode :- Get '[HTML] (Html ())
  , createPlan ::
      mode :- "plans"
        :> ReqBody '[FormUrlEncoded] PlanRequest
        :> Post '[HTML] (Html ())
  , showPlan ::
      mode :- "plan"
        :> Capture "planId" PlanID
        :> Get '[HTML] (Html ())
 } deriving stock (Generic)

-- | Routes that will never need a session cookie
data RoutesWithoutSession mode = RoutesWithoutSession
  { login ::
      mode :- "oauth" :> "github"
        :> QueryParam "return_to" Text
        :> Get302 '[HTML] '[Header "Set-Cookie" SetCookie] NoContent
  , githubCallback ::
      mode :- "oauth" :> "github" :> "callback"
        :> Header "Cookie" Text
        :> StrictParam "code" OAuthCode
        :> StrictParam "state" OAuthState
        :> Get302 '[HTML] '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent
  } deriving stock (Generic)

data PlanRequest = PlanRequest
  { planParam :: NonEmptyText
  , queryParam :: Maybe NonEmptyText
  }

data Session = Session
  { sessionGHUsername :: Maybe Text
  -- TODO: add user id?
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Session
instance ToJWT Session
instance FromJSON Session
instance FromJWT Session

-- cf:
-- https://stackoverflow.com/a/39819730
-- https://hackage.haskell.org/package/http-api-data-0.4.1.1/docs/Web-Internal-FormUrlEncoded.html#t:FromForm
instance FromForm PlanRequest where
  fromForm f =
    let parsedPlan = parseUnique "plan" f
        parsedQuery =
          -- NOTE: showing a bit of leniency to browsers which send this
          -- as an empty string.
          case parseUnique "query" f of
            Left _ -> Right Nothing
            Right e -> Right $ Just e
     in PlanRequest <$> parsedPlan <*> parsedQuery

server :: AppM sig m => ToServant Routes' (AsServerT m)
server =
    -- NOTE: wasn't immediately obvious that I needed to call @genericServerT@ at every level
    -- until I saw this: https://github.com/haskell-servant/servant/issues/1015
    genericServerT $ Routes'
      { routesWithSession = \authResult -> genericServerT $ RoutesWithSession
          { home = homeHandler authResult
          , createPlan = createPlanHandler
          , showPlan = showPlanHandler
          }
      , routesWithoutSession = genericServerT $ RoutesWithoutSession
          { login = loginHandler
          , githubCallback = githubCallbackHandler
          }
      }

loginHandler ::
  AppM sig m =>
  Maybe Text ->
  m (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
loginHandler returnTo = do
  AppContext{ctxGithubOAuthCredentials} <- ask
  oAuthState <- Crypto.randomUUID
  let loginUrl = mkIdentityUrl ctxGithubOAuthCredentials (OAuthState oAuthState) returnTo
      stateCookie =
        defaultSetCookie
          { setCookieName = "STATE"
          , setCookieValue = encodeUtf8 . UUID.toText $ oAuthState
          , setCookiePath = Just "/"
          -- NOTE: we _do_ want a cross-site cookie, to survive the redirection to
          -- and from Github
          , setCookieSameSite = Just sameSiteNone
          , setCookieHttpOnly = True
          , setCookieMaxAge = Just $ 10 * 60 -- 10 minutes
          , setCookieSecure = True
          }
  -- TODO: actually assign a random OAuth state and store it in a session cookie
  pure $ addHeader loginUrl $ addHeader stateCookie NoContent

githubCallbackHandler ::
  AppM sig m =>
  Maybe Text ->
  OAuthCode ->
  OAuthState ->
  m (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
githubCallbackHandler rawCookies oAuthCode oAuthState = do
  AppContext{ctxGithubOAuthCredentials,ctxJwtSettings,ctxCookieSettings} <- ask
  let cookies = parseCookiesText . encodeUtf8 <$> rawCookies
      stateCookie = lookup "STATE" =<< cookies
      mState = case stateCookie of
        Nothing -> Nothing
        Just stateText -> OAuthState <$> UUID.fromText stateText
  if not $ stateMatches mState oAuthState
    then throwError $ err500{errBody = "Invalid OAuth State"}
    else do
      oAuthResponse <- runGithubApiJSON $ getAccessToken ctxGithubOAuthCredentials oAuthCode
      case oAuthResponse of
        Left (JsonParseError e) -> do
          log Error $ "Error during oauth: " <> pack e
          throwError $ err500 {errBody = "Invalid OAuth Response"}
        Right oAuthData -> do
          log Debug $ "OAuth response: " <> pack (show oAuthData)
          let session = Session (Just "luis")
          mApplyCookies <- Crypto.acceptLogin ctxCookieSettings ctxJwtSettings session
          case mApplyCookies of
            Nothing -> throwError err401
            Just applyCookies -> pure $ addHeader "/" $ applyCookies NoContent
  where
    stateMatches Nothing _ = False
    stateMatches (Just sessionState) ghState = sessionState == ghState

homeHandler ::
  AppM sig m =>
  Auth.AuthResult Session ->
  m (Html ())
homeHandler sesh = do
  log Debug $ pack $ show sesh
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
    Nothing -> throwError $ htmlError err404 PEV2.planNotFound
    Just (_, pSource, pQuery, createdAt) ->
      renderView $ PEV2.page pSource pQuery createdAt

renderView :: AppM sig m => Html () -> m (Html ())
renderView = pure . Layout.layout

redirect :: AppM sig m => Text -> m (Html ())
redirect loc = do
  throwError $ err301{errHeaders = [("Location", encodeUtf8 loc)]}

htmlError :: ServerError -> Html () -> ServerError
htmlError err content =
  err{errBody = renderBS (Layout.layout content), errHeaders = [("Content-Type", "text/html")]}
