{-# LANGUAGE ConstraintKinds #-}

-- |
module PostgresExplainVisualizer.Types where

import PostgresExplainVisualizer.Effects.Database
import PostgresExplainVisualizer.Environment
import PostgresExplainVisualizer.Import
import Servant (ServerError)

type AppM sig m =
  ( Has (Throw ServerError) sig m
  , Has (Reader AppContext) sig m
  , Has Database sig m
  )
