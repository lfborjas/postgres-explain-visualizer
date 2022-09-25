{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic HTTP Effect

module PostgresExplainVisualizer.Effects.Http where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Error.Either (Throw, throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Data.Aeson.Types
import Data.Kind (Type)
import Control.Applicative (Alternative)
import Control.Monad.IO.Class (MonadIO (liftIO))

data Http (m :: Type -> Type) k where
  SendRequest :: HTTP.Request -> Http m (HTTP.Response L.ByteString)

sendRequest :: Has Http sig m => HTTP.Request -> m (HTTP.Response L.ByteString)
sendRequest r = send (SendRequest r)

newtype HttpClient m a = HttpClient {runHttp :: m a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadIO,
      Alternative,
      MonadFail
    )

instance (MonadIO m, Algebra sig m) => Algebra (Http :+: sig) (HttpClient m) where
  alg hdl sig ctx = case sig of
    L (SendRequest req) -> (<$ ctx) <$> liftIO (HTTP.getGlobalManager >>= HTTP.httpLbs req)
    R other -> HttpClient (alg (runHttp . hdl) other ctx)

newtype JsonParseError = JsonParseError String
  deriving (Show, Eq)

decodeOrThrow :: (Has (Throw JsonParseError) sig m, FromJSON a) => L.ByteString -> m a
decodeOrThrow = either (throwError . JsonParseError) pure . eitherDecode

-- | Same as `decodeOrThrow`, but takes an explicit parser vs. invoking the FromJSON instance of
-- the requested type.
decodeOrThrowWithParser :: (Has (Throw JsonParseError) sig m) => (Value -> Parser a) -> L.ByteString -> m a
decodeOrThrowWithParser parser bs =
  either (throwError . JsonParseError) pure (parseEither parser =<< eitherDecode bs)
