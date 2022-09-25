{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
module PostgresExplainVisualizer.Server.Pages where

import Control.Carrier.Error.Either (throwError)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Lucid (Html, renderBS)
import PostgresExplainVisualizer.Effects.Database (insert, select, update)
import PostgresExplainVisualizer.Models.Plan (
  PlanID,
  newPlan,
  planByID, claimPlans, plansByUserID
 )
import PostgresExplainVisualizer.Types (AppM)
import PostgresExplainVisualizer.Views.Layout qualified as Layout
import PostgresExplainVisualizer.Views.NewPlan qualified as NewPlan
import PostgresExplainVisualizer.Views.PEV2 qualified as PEV2
import Servant (
  Capture,
  FormUrlEncoded,
  Get,
  ReqBody,
  type (:>), Verb, StdMethod (GET, POST), Headers, Header, NoContent (NoContent), QueryParam, QueryParam', Required, Strict, addHeader, err401, err403
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
    ( getAccessToken,
      runGithubApiJSON,
      mkIdentityUrl,
      OAuthResponse(OAuthResponse, accessToken),
      OAuthState(OAuthState),
      OAuthCode,
      GithubUser(GithubUser),
      getUser )
import PostgresExplainVisualizer.Environment (AppContext(..))
import Control.Carrier.Reader (ask)
import PostgresExplainVisualizer.Effects.Http
    ( JsonParseError(JsonParseError) )
import PostgresExplainVisualizer.Effects.Log
    ( log, LogLevel(Error, Debug) )
import Prelude hiding (log)
import Servant.Auth.Server ( Auth, SetCookie)
import Servant.Auth.Server qualified as Auth
import PostgresExplainVisualizer.Effects.Crypto qualified as Crypto
import qualified PostgresExplainVisualizer.Client.Github.Api as GH
import PostgresExplainVisualizer.Models.Common (unsafeNonEmptyText, NonEmptyText)
import PostgresExplainVisualizer.Models.User (UserID, userByGithubUsername, newUser)
import Control.Monad (void)
import PostgresExplainVisualizer.Server.Types
import Control.Applicative ((<|>))
import qualified PostgresExplainVisualizer.Views.PlanList as PlanList

type Routes = ToServantApi Routes'
type Get302 (cts :: [Type]) (hs :: [Type]) a = Verb 'GET 302 cts (Headers (Header "Location" Text ': hs) a)
type Post302 (cts :: [Type]) (hs :: [Type]) a = Verb 'POST 302 cts (Headers (Header "Location" Text ': hs) a)
type StrictParam = QueryParam' '[Required, Strict]

data Routes' mode = Routes'
  { routesWithSession :: mode :- Auth '[Auth.Cookie] Session :> ToServantApi RoutesWithSession
  , routesWithoutSession :: mode :- ToServantApi RoutesWithoutSession
  }
  deriving stock (Generic)

-- | Routes that may use a session cookie when present
data RoutesWithSession mode = RoutesWithSession
  { createPlan ::
      mode :- "plans"
        :> ReqBody '[FormUrlEncoded] PlanRequest
        :> Post302 '[HTML] '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent
  , showPlan ::
      mode :- "plan"
        :> Capture "planId" PlanID
        :> Get '[HTML] (Html ())
  , listOwnPlans ::
      mode :- "plans"
        :> Get '[HTML] (Html ())
  , login ::
      mode :- "oauth" :> "github"
        :> QueryParam "return_to" Text
        :> Get302 '[HTML] '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent
  , githubCallback ::
      mode :- "oauth" :> "github" :> "callback"
        :> StrictParam "code" OAuthCode
        :> StrictParam "state" OAuthState
        :> Get302 '[HTML] '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent

 } deriving stock (Generic)

-- | Routes that will never need a session cookie
data RoutesWithoutSession mode = RoutesWithoutSession
  { home :: mode :- Get '[HTML] (Html ())
  } deriving stock (Generic)

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
          { login = loginHandler authResult
          , githubCallback = githubCallbackHandler authResult
          , createPlan = createPlanHandler authResult
          , showPlan = showPlanHandler authResult
          , listOwnPlans = listOwnPlansHandler authResult
          }
      , routesWithoutSession = genericServerT $ RoutesWithoutSession
          {home = homeHandler
          }
      }


loginHandler ::
  AppM sig m =>
  Auth.AuthResult Session ->
  Maybe Text ->
  m (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler (Auth.Authenticated session@(Authenticated _)) _ = do
  applySessionAndRedirect session "/"
loginHandler authResult returnTo = do
  AppContext{ctxGithubOAuthCredentials, ctxCookieSettings , ctxJwtSettings} <- ask
  oAuthState <- OAuthState <$> Crypto.randomUUID
  let loginUrl = mkIdentityUrl ctxGithubOAuthCredentials oAuthState
      modifiedCookieSettings = ctxCookieSettings{Auth.cookieSameSite = Auth.AnySite}
      anonSession = updateExistingAnonSession authResult oAuthState
  mApplyCookies <- Crypto.acceptLogin modifiedCookieSettings ctxJwtSettings anonSession
  case mApplyCookies of
    Nothing -> throwError err500{errBody = "Unable to initiate login"}
    Just applyCookies -> pure $ addHeader loginUrl $ applyCookies NoContent
  where
    updateExistingAnonSession authResult' oAuth = case authResult' of
      Auth.Authenticated (Anonymous (AnonymousData existingPlans _ existingReturnTo)) ->
        Anonymous (AnonymousData existingPlans (Just oAuth) (existingReturnTo <|> returnTo))
      _ ->
        Anonymous (AnonymousData [] (Just oAuth) returnTo)

githubCallbackHandler ::
  AppM sig m =>
  Auth.AuthResult Session ->
  OAuthCode ->
  OAuthState ->
  m (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
githubCallbackHandler (Auth.Authenticated session@(Authenticated UserSessionData{})) _ _  = do
  applySessionAndRedirect session "/"
githubCallbackHandler (Auth.Authenticated (Anonymous AnonymousData{oAuthNegotiationState, unclaimedPlans, returnToAfterLogin})) oAuthCode oAuthState = do
  AppContext{ctxGithubOAuthCredentials,ctxJwtSettings,ctxCookieSettings} <- ask
  if not $ stateMatches oAuthNegotiationState oAuthState
    then throwError $ err500{errBody = "Invalid OAuth State"}
    else do
      oAuthResponse <- runGithubApiJSON $ getAccessToken ctxGithubOAuthCredentials oAuthCode
      case oAuthResponse of
        Left (JsonParseError e) -> do
          log Error $ "Error during oauth: " <> pack e
          throwError $ err500 {errBody = "Invalid OAuth Response"}
        Right OAuthResponse{accessToken} -> do
          userResponse <- runGithubApiJSON $ getUser accessToken
          case userResponse of
            Left (JsonParseError e) -> do
              log Error $ "Error getting user info: " <> pack e
              throwError $ err500 {errBody = "Unable to verify identity"}
            Right GithubUser{GH.login = githubUsername} -> do
              userId <- upsertUser $ unsafeNonEmptyText githubUsername
              let session = Authenticated $ UserSessionData githubUsername userId
              mApplyCookies <- Crypto.acceptLogin ctxCookieSettings ctxJwtSettings session
              case mApplyCookies of
                Nothing -> throwError err401
                Just applyCookies -> do
                  void $ update $ claimPlans userId unclaimedPlans
                  pure $ addHeader (fromMaybe "/" returnToAfterLogin) $ applyCookies NoContent
  where
    stateMatches Nothing _ = False
    stateMatches (Just state) otherState = state == otherState
    upsertUser :: AppM sig m => NonEmptyText -> m UserID
    upsertUser uname = do
      created <- insert $ newUser uname
      case listToMaybe created of
        Just (createdId, _) -> pure createdId
        Nothing -> do
          existing <- select $ userByGithubUsername uname
          case listToMaybe existing of
            Nothing -> throwError $ err500{errBody = "Unable to authenticate"}
            Just (existingId, _) -> pure existingId
githubCallbackHandler _ _ _ =
  throwError err403{errBody = "Invalid authentication state"}

homeHandler ::
  AppM sig m =>
  m (Html ())
homeHandler = do
  renderView $ NewPlan.page Nothing

createPlanHandler ::
  AppM sig m =>
  Auth.AuthResult Session ->
  PlanRequest ->
  m (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
createPlanHandler authResult PlanRequest{planParam, queryParam} = do
  let currentUserData = getCurrentUserData authResult
  createdPlan <- insert $ newPlan planParam queryParam (currentUserId <$> currentUserData)
  case listToMaybe createdPlan of
    Nothing -> throwError $ err500{errBody = "Unable to store plan, sorry!"}
    Just (createdId, _, _) -> do
      newSession <- updateUnclaimedPlans authResult createdId
      applySessionAndRedirect newSession $ "/plan/" <> (pack . show $ createdId)

showPlanHandler ::
  AppM sig m =>
  Auth.AuthResult Session ->
  PlanID ->
  m (Html ())
showPlanHandler authResult pid = do
  let mSessionData = getCurrentUserData authResult
  plan <- select $ planByID pid
  case listToMaybe plan of
    Nothing -> throwError $ htmlError err404 $ PEV2.planNotFound mSessionData pid
    Just (_, pSource, pQuery, createdAt) ->
      renderView $ PEV2.page mSessionData pid pSource pQuery createdAt

listOwnPlansHandler ::
  AppM sig m =>
  Auth.AuthResult Session ->
  m (Html ())
listOwnPlansHandler (Auth.Authenticated (Authenticated UserSessionData {currentUserId})) = do
  myPlans <- select $ plansByUserID currentUserId
  let planViews = map (\(i, s, q, created) -> PlanView i s q created) myPlans
  renderView $ PlanList.page planViews

listOwnPlansHandler _ = redirect "/"
-- HELPERS

renderView :: AppM sig m => Html () -> m (Html ())
renderView = pure . Layout.layout

redirect :: AppM sig m => Text -> m (Html ())
redirect loc = do
  throwError $ err301{errHeaders = [("Location", encodeUtf8 loc)]}

htmlError :: ServerError -> Html () -> ServerError
htmlError err content =
  err{errBody = renderBS (Layout.layout content), errHeaders = [("Content-Type", "text/html")]}

applySessionAndRedirect ::
  AppM sig m =>
  Session ->
  Text ->
  m (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
applySessionAndRedirect session redirectTo = do
  AppContext{ctxJwtSettings,ctxCookieSettings} <- ask
  mApplyCookies <- Crypto.acceptLogin ctxCookieSettings ctxJwtSettings session
  case mApplyCookies of
    Nothing -> throwError err401
    Just applyCookies -> pure $ addHeader redirectTo $ applyCookies NoContent

updateUnclaimedPlans :: AppM sig m => Auth.AuthResult Session -> PlanID -> m Session
updateUnclaimedPlans authResult planId = case authResult of
  Auth.Authenticated session@(Authenticated _) -> pure session
  Auth.Authenticated (Anonymous AnonymousData{unclaimedPlans, returnToAfterLogin}) ->
    pure $ Anonymous $ AnonymousData (planId : unclaimedPlans) Nothing returnToAfterLogin
  _ -> pure defaultAnonymous
  where
    defaultAnonymous = Anonymous AnonymousData{unclaimedPlans = [planId], oAuthNegotiationState = Nothing, returnToAfterLogin = Nothing}

getCurrentUserData :: Auth.AuthResult Session -> Maybe UserSessionData
getCurrentUserData = \case
  Auth.Authenticated (Authenticated userData) -> Just userData
  Auth.Authenticated _ -> Nothing
  _ -> Nothing
