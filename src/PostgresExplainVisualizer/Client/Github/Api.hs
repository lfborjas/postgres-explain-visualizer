{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Effects to talk to the GH API

module PostgresExplainVisualizer.Client.Github.Api
  ( runGithubApiJSON
  , getAccessToken
  , getUser
  , mkIdentityUrl
  , OAuthCode (..)
  , OAuthState (..)
  , OAuthResponse (..)
  , GithubUser(..)
  )
where

import Control.Algebra
import Control.Carrier.Error.Either
import PostgresExplainVisualizer.Effects.Http
    ( JsonParseError, Http, sendRequest, decodeOrThrow )
import Network.HTTP.Client qualified as HTTP
import Data.Kind (Type)
import PostgresExplainVisualizer.Environment (GithubOAuthCredentials (..))
import Data.Text
import Data.List.NonEmpty (NonEmpty(..), toList)
import GHC.Generics (Generic)
import Data.Aeson
import Data.UUID ( UUID, toText )
import Data.String (IsString)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative (Alternative)
import Data.Function ((&))
import Data.Text.Encoding (encodeUtf8)
import Web.HttpApiData (FromHttpApiData)
import Servant.Auth.Server (ToJWT, FromJWT)
import Servant.API.Empty (EmptyAPI)

newtype OAuthCode =
  OAuthCode {unOAuthCode :: Text}
  deriving stock (Show)
  deriving newtype (FromJSON, FromHttpApiData)

newtype AccessToken =
  AccessToken {unAccessToken :: Text}
  deriving stock (Show)
  deriving newtype (FromJSON)

-- NOTE: should this be a sum type? Not sure if we care about the value here,
-- but it _should_ be some thing known, like `Bearer`
newtype OAuthScope =
  OAuthScope {unScope :: Text}
  deriving stock (Show)
  deriving newtype (FromJSON, IsString)

newtype OAuthTokenType =
  OAuthTokenType {unTokenType :: Text}
  deriving stock (Show)
  deriving newtype (FromJSON)

newtype OAuthState =
  OAuthState {unOAuthState :: UUID}
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype (FromHttpApiData)

instance ToJSON OAuthState
instance ToJWT OAuthState
instance FromJSON OAuthState
instance FromJWT OAuthState

-- | Response when exchanging a code for an access token:
-- https://docs.github.com/en/developers/apps/building-oauth-apps/authorizing-oauth-apps#response
data OAuthResponse = OAuthResponse
  { accessToken :: AccessToken
  , scope :: [OAuthScope] --  TODO: should this be NonEmpty, too?
  , tokenType :: OAuthTokenType
  }
  deriving stock (Show, Generic)

instance FromJSON OAuthResponse where
  parseJSON = withObject "OAuthResponse" $ \v -> do
    accessToken <- v .: "access_token"
    tokenType <- v .: "token_type"
    scopes <- v .: "scope"
    let scope = OAuthScope <$> splitOn "," scopes
    pure  $ OAuthResponse{..}

-- | Subset of user data available through the GH API
-- https://docs.github.com/en/rest/users/users#get-the-authenticated-user
data GithubUser = GithubUser
  { login :: Text
  , gravatarId :: Maybe Text
  , name :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON GithubUser where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

-- API Helpers

githubOAuthScopes :: NonEmpty OAuthScope
githubOAuthScopes = "read:user" :| ["read:org"]

-- | Construct a URL to redirect to to initiate GH authentication
-- https://docs.github.com/en/developers/apps/building-oauth-apps/authorizing-oauth-apps#1-request-a-users-github-identity
mkIdentityUrl :: GithubOAuthCredentials -> OAuthState -> Maybe Text -> Text
mkIdentityUrl GithubOAuthCredentials {clientId} state returnTo =
  mconcat
    [ "https://github.com/login/oauth/authorize"
    , "?client_id="
    , clientId
    , "&scope="
    , intercalate "%20" $ unScope <$> toList githubOAuthScopes
    , "&state="
    , toText . unOAuthState $ state
    , maybe "" ("&redirect_uri=" <>) returnTo
    ]

-- API algebra

data GithubClient (m :: Type -> Type) k where
  -- See:
  GetAccessToken :: GithubOAuthCredentials -> OAuthCode -> GithubClient m OAuthResponse
  -- See: https://docs.github.com/en/rest/users/users#get-the-authenticated-user
  -- https://docs.github.com/en/rest/orgs/orgs#list-organizations-for-the-authenticated-user
  GetUser :: AccessToken -> GithubClient m GithubUser

getAccessToken :: Has GithubClient sig m => GithubOAuthCredentials -> OAuthCode -> m OAuthResponse
getAccessToken creds code = send $ GetAccessToken creds code

getUser :: Has GithubClient sig m => AccessToken -> m GithubUser
getUser = send . GetUser

newtype GithubApi m a =
  GithubApi { runGithubApi :: m a }
  deriving (Monad, Functor, Applicative, MonadIO, MonadFail, Alternative)

-- | Get a GH access token:
-- https://docs.github.com/en/developers/apps/building-oauth-apps/authorizing-oauth-apps#2-users-are-redirected-back-to-your-site-by-github
accessTokenRequest :: GithubOAuthCredentials -> OAuthCode -> HTTP.Request
accessTokenRequest GithubOAuthCredentials {clientId, clientSecret} code =
  let endpoint = HTTP.parseRequest_ "https://github.com/login/oauth/access_token"
      params = [ ("client_id", encodeUtf8 clientId)
               , ("client_secret", encodeUtf8 clientSecret)
               , ("code", encodeUtf8 . unOAuthCode $ code)
               -- NOTE: apparently "redirect_uri" is also permissible here?
               ]
      headers = [("Accept", "application/json")]
      req = endpoint & HTTP.urlEncodedBody params
   in req{HTTP.method = "POST", HTTP.requestHeaders = headers}

githubAPIBase :: Text
githubAPIBase = "https://api.github.com"

authenticatedJSONRequest :: AccessToken -> HTTP.Request -> HTTP.Request
authenticatedJSONRequest (AccessToken token) req =
  req{HTTP.requestHeaders = extraHeaders <> HTTP.requestHeaders req}
  where
    accessTokenHeader = ("Authorization", encodeUtf8 $ "Bearer " <> token)
    acceptHeader = ("Accept", "application/json")
    -- NOTE: mandated by GH, otherwise we get an error!
    userAgent = ("User-Agent", "pgexplain.dev")
    extraHeaders = [accessTokenHeader, acceptHeader, userAgent]

getUserRequest :: HTTP.Request
getUserRequest =
  let endpoint = HTTP.parseRequest_ . unpack $ githubAPIBase <> "/user"
   in endpoint{HTTP.method = "GET"}

instance (Has Http sig m, Has (Throw JsonParseError) sig m, Algebra sig m) => Algebra (GithubClient :+: sig) (GithubApi m) where
  alg hdl sig ctx = case sig of
    L (GetAccessToken creds code) -> do
      resp <- sendRequest $ accessTokenRequest creds code
      (<$ ctx) <$> decodeOrThrow (HTTP.responseBody resp)
    L (GetUser token) -> do
      resp <- sendRequest $ authenticatedJSONRequest token getUserRequest
      (<$ ctx) <$> decodeOrThrow (HTTP.responseBody resp)
    R other -> GithubApi (alg (runGithubApi . hdl) other ctx)

runGithubApiJSON :: GithubApi (ErrorC JsonParseError m) a -> m (Either JsonParseError a)
runGithubApiJSON = (runError @JsonParseError) . runGithubApi
