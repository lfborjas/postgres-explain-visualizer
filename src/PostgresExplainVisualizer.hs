module PostgresExplainVisualizer where

import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  progDesc,
  switch,
 )
import PostgresExplainVisualizer.Database.Migrations (runMigrations)
import PostgresExplainVisualizer.Environment (
  Config (configDatabaseUrl),
  getServerConfig,
 )
import PostgresExplainVisualizer.Server qualified as Server

newtype Opts = Opts {optMigrate :: Bool}

run :: IO ()
run = do
  config <- getServerConfig
  opts <- execParser optsParser
  if optMigrate opts
    then do
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
