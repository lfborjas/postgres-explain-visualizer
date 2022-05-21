{-# LANGUAGE ImportQualifiedPost #-}
module PostgresExplainVisualizer where

import PostgresExplainVisualizer.Server qualified as Server
import Options.Applicative
import PostgresExplainVisualizer.Environment
import PostgresExplainVisualizer.Database.Migrations (runMigrations)

newtype Opts = Opts {optMigrate :: Bool}

run :: IO ()
run = do
  config <- getServerConfig
  opts <- execParser optsParser
  if optMigrate opts then do
    putStrLn "Migrating..."
    didMigrate <- runMigrations "migrations" (configDatabaseUrl config)
    case didMigrate of
      Left e -> putStrLn $ "Error migrating: " <> e
      Right s -> putStrLn s
  else do
    Server.run config

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> mainOptions)
    (fullDesc <> progDesc "Run server, or migrate")

mainOptions :: Parser Opts
mainOptions =
  Opts <$> switch (long "migrate" <> help "Init migrations table idempotently, then run missing migrations")
