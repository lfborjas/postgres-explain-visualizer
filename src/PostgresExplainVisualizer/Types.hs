-- |
{-#LANGUAGE ConstraintKinds #-}

module PostgresExplainVisualizer.Types where

import PostgresExplainVisualizer.Import
import Servant (ServerError)
import PostgresExplainVisualizer.Environment
import PostgresExplainVisualizer.Effects.Database


type AppM sig m =
  (
    Has (Throw ServerError) sig m,
    Has (Reader AppContext) sig m,
    Has Database sig m
  )
