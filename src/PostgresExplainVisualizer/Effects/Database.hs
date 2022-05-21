{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module PostgresExplainVisualizer.Effects.Database where

import Control.Algebra
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Data.Kind
import Data.Profunctor.Product.Default qualified as D
import Database.PostgreSQL.Simple qualified as PG
import Opaleye qualified as DB
import Opaleye.Internal.Inferrable qualified as DBI

data Database (m :: Type -> Type) k where
  -- rel8 world
  Insert :: DB.Insert hs -> Database m hs
  Update :: DB.Update hs -> Database m hs
  Delete :: DB.Delete hs -> Database m hs
  Select :: D.Default (DBI.Inferrable DB.FromFields) fields hs => DB.Select fields -> Database m [hs]

insert :: Has Database sig m => DB.Insert hs -> m hs
insert = send . Insert

update :: Has Database sig m => DB.Update hs -> m hs
update = send . Update

delete :: Has Database sig m => DB.Delete hs -> m hs
delete = send . Delete

select :: (D.Default (DBI.Inferrable DB.FromFields) fields hs, Has Database sig m) => DB.Select fields -> m [hs]
select = send . Select

-- CARRIERS

newtype DatabaseIOC m a = DatabaseIOC {runDatabaseIO :: ReaderC PG.Connection m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

runDatabaseWithConnection :: PG.Connection -> DatabaseIOC m hs -> m hs
runDatabaseWithConnection conn = runReader conn . runDatabaseIO

instance
  (MonadIO m, Algebra sig m) =>
  Algebra (Database :+: sig) (DatabaseIOC m)
  where
  alg hdl sig ctx = DatabaseIOC $ case sig of
    L (Insert query') -> do
      conn <- ask
      (<$ ctx) <$> liftIO (DB.runInsert_ conn query')
    L (Update query') -> do
      conn <- ask
      (<$ ctx) <$> liftIO (DB.runUpdate_ conn query')
    L (Delete query') -> do
      conn <- ask
      (<$ ctx) <$> liftIO (DB.runDelete_ conn query')
    L (Select query') -> do
      conn <- ask
      (<$ ctx) <$> liftIO (DB.runSelectI conn query')
