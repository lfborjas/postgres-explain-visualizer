{-# LANGUAGE ImportQualifiedPost #-}
module PostgresExplainVisualizer where

import PostgresExplainVisualizer.Server qualified as Server

run :: IO ()
run = Server.run
