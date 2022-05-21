{-# LANGUAGE ConstraintKinds #-}

-- |
module PostgresExplainVisualizer.Types where

import PostgresExplainVisualizer.Effects.Database ( Database )
import PostgresExplainVisualizer.Environment ( AppContext )
import Servant (ServerError)
import Control.Algebra ( Has )
import Control.Carrier.Error.Either ( Throw )
import Control.Carrier.Reader ( Reader )

-- | Useful type synonym for handlers; other specialized operations (like database helpers,)
-- should only use the constraints that they need.
type AppM sig m =
  ( Has (Throw ServerError) sig m
  , Has (Reader AppContext) sig m
  , Has Database sig m
  )
