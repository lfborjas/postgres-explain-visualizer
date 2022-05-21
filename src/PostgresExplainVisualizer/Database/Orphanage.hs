-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgresExplainVisualizer.Database.Orphanage where

import Data.Profunctor.Product.Default qualified as D
import Data.Time qualified as Time
import Opaleye (FromField)
import Opaleye qualified as T
import Opaleye.Internal.Inferrable

-- This is still not fully settled in the Opaleye repo:
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/495
instance utcTime ~ Time.UTCTime
  => D.Default (Inferrable FromField) T.SqlTimestamptz utcTime where
  def = Inferrable D.def
