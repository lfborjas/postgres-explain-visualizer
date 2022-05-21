-- |
module PostgresExplainVisualizer.Database.Migrations where

import Control.Monad (void)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), MigrationResult (..), runMigration)

-- TODO(luis) we may want to take a Bool parameter to send
-- in `MigrationContext`: right now it defaults to verbose.
runMigrations' :: Bool -> FilePath -> Text -> IO (Either String String)
runMigrations' isVerbose migrationsDir conStr = do
  con <- connectPostgreSQL $ encodeUtf8 conStr
  -- initialize the `schema_migrations` table
  void $
    withTransaction con $
      runMigration $
        MigrationContext MigrationInitialization isVerbose con
  -- run the actual migrations
  result <-
    withTransaction con $
      runMigration $
        MigrationContext
          (MigrationDirectory migrationsDir)
          isVerbose
          con

  case result of
    MigrationSuccess -> pure $ Right "All migrations ran."
    MigrationError e -> pure $ Left e

-- | run migrations verbosely in the given directory, connected to the given
-- DB URL
runMigrations :: FilePath -> Text -> IO (Either String String)
runMigrations = runMigrations' True

-- | Same as `runMigrations`, but without output.
runMigrationsSilent :: FilePath -> Text -> IO (Either String String)
runMigrationsSilent = runMigrations' False
