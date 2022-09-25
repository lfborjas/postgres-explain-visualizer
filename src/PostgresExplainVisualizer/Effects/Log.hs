{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Simple logging

module PostgresExplainVisualizer.Effects.Log where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Data.Kind (Type)
import Data.Text
import Control.Monad.IO.Class (MonadIO(liftIO))

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Eq, Ord)

-- NOTE: we could use a type variable instead of Text if we wanted e.g. structured logs
data Log (m :: Type -> Type) k where
  Log :: LogLevel -> Text -> Log m ()

log :: (Has Log sig m) => LogLevel -> Text -> m ()
log level txt = send $ Log level txt

newtype LogStdoutC m a = LogStdoutC{runLogStdout :: m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

instance (MonadIO m, Algebra sig m) => Algebra (Log :+: sig) (LogStdoutC m) where
  alg hdl sig ctx = case sig of
    L (Log level message) ->
      (<$ ctx) <$> liftIO (putStrLn $ renderLogMessage level message)
    R other -> LogStdoutC (alg (runLogStdout . hdl) other ctx)

renderLogMessage :: LogLevel -> Text -> String
renderLogMessage level message =
  mconcat
    [ "["
    , show level
    , "] "
    , unpack message
    ]
