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
  , mkIdentityUrl
  , OAuthCode (..)
  , OAuthState (..)
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
  deriving stock (Eq)
  deriving newtype (FromHttpApiData)

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

getAccessToken :: Has GithubClient sig m => GithubOAuthCredentials -> OAuthCode -> m OAuthResponse
getAccessToken creds code = send $ GetAccessToken creds code

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

instance (Has Http sig m, Has (Throw JsonParseError) sig m, Algebra sig m) => Algebra (GithubClient :+: sig) (GithubApi m) where
  alg hdl sig ctx = case sig of
    L (GetAccessToken creds code) -> do
      resp <- sendRequest $ accessTokenRequest creds code
      (<$ ctx) <$> decodeOrThrow (HTTP.responseBody resp)
    R other -> GithubApi (alg (runGithubApi . hdl) other ctx)

runGithubApiJSON :: GithubApi (ErrorC JsonParseError m) a -> m (Either JsonParseError a)
runGithubApiJSON = (runError @JsonParseError) . runGithubApi
