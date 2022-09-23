{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}


-- | Generating random UUIDs, signing JWT, all kinds of cryptographical bs.

module PostgresExplainVisualizer.Effects.Crypto where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Data.Kind (Type)
import Servant.Auth.Server
import Servant
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant.Auth.Server qualified as Servant
import Data.UUID
import Data.UUID.V4 qualified as UUID

data Crypto (m :: Type -> Type) k where
  AcceptLogin ::
    (ToJWT session
    , AddHeader "Set-Cookie" SetCookie response withOneCookie
    , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies) =>
    CookieSettings ->
    JWTSettings ->
    session ->
    Crypto m (Maybe (response -> withTwoCookies))
  RandomUUID :: Crypto m UUID

acceptLogin ::
  (ToJWT session
  , AddHeader "Set-Cookie" SetCookie response withOneCookie
  , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies
  , Has Crypto sig m) =>
  CookieSettings ->
  JWTSettings ->
  session ->
  m (Maybe (response -> withTwoCookies))
acceptLogin s c j = send $ AcceptLogin s c j

randomUUID :: Has Crypto sig m => m UUID
randomUUID = send RandomUUID

newtype CryptoIOC m a = CryptoIOC {runCryptoIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Crypto :+: sig) (CryptoIOC m) where
  alg hdl sig ctx = case sig of
    L (AcceptLogin cookie jwt session) ->
      (<$ ctx) <$> liftIO (Servant.acceptLogin cookie jwt session)
    L RandomUUID ->
      (<$ ctx) <$> liftIO UUID.nextRandom
    R other -> CryptoIOC $ alg (runCryptoIO . hdl) other ctx
