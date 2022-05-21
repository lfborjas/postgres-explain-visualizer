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
import Data.Text ( null, Text )
import Data.UUID ( UUID )
import Opaleye
    ( SqlTimestamptz,
      Insert(..),
      Select,
      toToFields,
      optionalTableField,
      requiredTableField,
      rReturning,
      (.===),
      where_,
      sqlStrictText,
      selectTable,
      table,
      toFields,
      Field,
      FieldNullable,
      Column,
      ToFields,
      OnConflict(DoNothing),
      SqlText,
      SqlUuid,
      DefaultFromField(..),
      Table,
      tableField )
import PostgresExplainVisualizer.Database.Orphanage ()
import PostgresExplainVisualizer.Models.Common
    ( pEntity,
      withTimestamp,
      withTimestampFields,
      Entity,
      EntityField,
      EntityT(Entity, record, recordCreatedAt),
      EntityWriteField )
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
newtype NonEmptyText = NonEmptyText Text

mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText t
  | Data.Text.null t = Nothing
  | otherwise = Just . NonEmptyText $ t

instance DefaultFromField SqlText NonEmptyText where
  defaultFromField = NonEmptyText <$> defaultFromField

instance Default ToFields NonEmptyText (Column SqlText) where
  def = toToFields (\(NonEmptyText txt) -> sqlStrictText txt)

instance FromHttpApiData NonEmptyText where
  parseUrlPiece t = do
    s <- parseUrlPiece t
    case mkNonEmptyText s of
      Nothing -> Left $ "Text cannot be empty " <> t
      Just parsed -> Right parsed

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
