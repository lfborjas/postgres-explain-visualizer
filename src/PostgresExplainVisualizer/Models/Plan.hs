{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module PostgresExplainVisualizer.Models.Plan where

import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text, null)
import Data.UUID (UUID)
import Opaleye (
  Column,
  DefaultFromField (..),
  Field,
  FieldNullable,
  Insert (..),
  OnConflict (DoNothing),
  Select,
  SqlText,
  SqlTimestamptz,
  SqlUuid,
  Table,
  ToFields,
  optionalTableField,
  rReturning,
  requiredTableField,
  selectTable,
  sqlStrictText,
  table,
  tableField,
  toFields,
  toToFields,
  where_,
  (.===),
 )
import PostgresExplainVisualizer.Database.Orphanage ()
import PostgresExplainVisualizer.Models.Common (
  Entity,
  EntityField,
  EntityT (Entity, record, recordCreatedAt),
  EntityWriteField,
  pEntity,
  withTimestamp,
  withTimestampFields, NonEmptyText
 )
import Web.Internal.HttpApiData (FromHttpApiData (..))

---------------------------------------------------------------------------------

newtype PlanID' a = PlanID {getPlanId :: a}
  deriving newtype (Eq, Show)
  deriving (Functor)

$(makeAdaptorAndInstanceInferrable "pPlanID" ''PlanID')

type PlanIDField = PlanID' (Field SqlUuid)
type PlanIDWrite = PlanID' (Maybe (Field SqlUuid))
type PlanID = PlanID' UUID

deriving via UUID instance (FromHttpApiData PlanID)

---------------------------------------------------------------------------------

data Plan' pid psource pquery = Plan
  { planID :: pid
  , planSource :: psource
  , planQuery :: pquery
  }

type Plan =
  Entity
    ( Plan'
        PlanID
        NonEmptyText
        (Maybe NonEmptyText)
    )

type PlanWrite =
  EntityWriteField
    ( Plan'
        PlanIDWrite
        (Field SqlText)
        (FieldNullable SqlText)
    )

type PlanField =
  EntityField
    ( Plan'
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
        , planQuery = tableField "query"
        }

---------------------------------------------------------------------------------

-- QUERIES

newPlan :: NonEmptyText -> Maybe NonEmptyText -> Insert [(PlanID, Text, Maybe Text)]
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

planByID :: PlanID -> Select (PlanIDField, Field SqlText, FieldNullable SqlText, Field SqlTimestamptz)
planByID pid_ = do
  Entity{record, recordCreatedAt} <- selectTable planTable
  where_ $ planID record .=== toFields pid_
  pure (planID record, planSource record, planQuery record, recordCreatedAt)
