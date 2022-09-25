{-# LANGUAGE ConstraintKinds #-}

-- |
module PostgresExplainVisualizer.Types where

import Control.Algebra (Has)
import Control.Carrier.Error.Either (Throw)
import Control.Carrier.Reader (Reader)
import PostgresExplainVisualizer.Effects.Database (Database)
import PostgresExplainVisualizer.Environment (AppContext)
import Servant (ServerError)
import PostgresExplainVisualizer.Effects.Http (Http)
import PostgresExplainVisualizer.Effects.Log (Log)
import PostgresExplainVisualizer.Effects.Crypto (Crypto)

-- | Useful type synonym for handlers; other specialized operations (like database helpers,)
-- should only use the constraints that they need.
type AppM sig m =
  ( Has (Throw ServerError) sig m
  , Has (Reader AppContext) sig m
  , Has Database sig m
  , Has Http sig m
  , Has Log sig m
  , Has Crypto sig m
  )

