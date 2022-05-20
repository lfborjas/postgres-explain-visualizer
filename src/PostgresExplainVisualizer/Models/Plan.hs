{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |

module PostgresExplainVisualizer.Models.Plan where

import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import PostgresExplainVisualizer.Models.Common
import Opaleye
import Data.Profunctor.Product.Default (Default(..))
import Data.UUID
import Data.Text
import Web.Internal.HttpApiData (FromHttpApiData)


---------------------------------------------------------------------------------

newtype PlanID' a = PlanID {getPlanId :: a}
  deriving newtype (Eq, Show)
  deriving Functor

$(makeAdaptorAndInstanceInferrable "pPlanID" ''PlanID')

type PlanIDField = PlanID' (Field SqlUuid)
type PlanIDWrite = PlanID' (Maybe (Field SqlUuid))
type PlanID = PlanID' UUID

deriving via UUID instance (FromHttpApiData PlanID)

---------------------------------------------------------------------------------

data Plan' pid psource pquery =
  Plan
    { planID :: pid
    , planSource :: psource
    , planQuery :: pquery
    }

type Plan =
  Entity
    (Plan'
      PlanID
      Text
      (Maybe Text)
    )

type PlanWrite =
  EntityWriteField
    (Plan'
      PlanIDWrite
      (Field SqlText)
      (FieldNullable SqlText)
    )

type PlanField =
  EntityField
    (Plan'
      PlanIDField
      (Field SqlText)
      (FieldNullable SqlText)
    )

$(makeAdaptorAndInstanceInferrable "pPlan" ''Plan')

---------------------------------------------------------------------------------

-- FIXME: the source and query should be nonempty text, maybe a newtype that could
-- potentially also be parsed as valid Plan/SQL output.
planTable ::
  Table PlanWrite PlanField
planTable =
  table "plan" . pEntity . withTimestampFields $
    pPlan
      Plan
        { planID = pPlanID (PlanID $ optionalTableField "id")
        , planSource = requiredTableField "source"
        , planQuery  = tableField "query"
        }


---------------------------------------------------------------------------------

-- QUERIES

newPlan :: Text -> Maybe Text -> Insert [(PlanID, Text, Maybe Text)]
newPlan source query =
  Insert
    { iTable = planTable
    , iRows = withTimestamp [row]
    , iReturning = rReturning (\Entity{record} -> (planID record, planSource record, planQuery record))
    , iOnConflict = Just DoNothing
    }
  where
    row =
      Plan
        (PlanID Nothing)
        (toFields source)
        (toFields query)

planByID :: PlanID -> Select (PlanIDField, Field SqlText, FieldNullable SqlText)
planByID pid_ = do
  Entity{record} <- selectTable planTable
  where_ $ planID record .=== toFields pid_
  pure (planID record, planSource record, planQuery record)
