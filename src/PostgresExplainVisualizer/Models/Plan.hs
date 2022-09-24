{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module PostgresExplainVisualizer.Models.Plan where

import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.UUID (UUID)
import Opaleye (
  Field,
  FieldNullable,
  Insert (..),
  Update (..),
  OnConflict (DoNothing),
  Select,
  SqlText,
  SqlTimestamptz,
  SqlUuid,
  Table,
  optionalTableField,
  rReturning,
  requiredTableField,
  selectTable,
  table,
  tableField,
  toFields,
  where_,
  (.===), maybeToNullable, Update (uUpdateWith), SqlBool, (.&&), in_, toFieldsI
 )
import PostgresExplainVisualizer.Database.Orphanage ()
import PostgresExplainVisualizer.Models.Common (
  Entity,
  EntityField,
  EntityT (Entity, record, recordCreatedAt),
  EntityWriteField,
  pEntity,
  withTimestamp,
  withTimestampFields, NonEmptyText, updateRecordWith
 )
import Web.Internal.HttpApiData (FromHttpApiData (..))
import PostgresExplainVisualizer.Models.User (UserID, UserID' (UserID, getUserId), pUserID)
import Data.Aeson (ToJSON, FromJSON)
import Servant.Auth.Server (ToJWT, FromJWT)
import Opaleye.Column (isNull)

---------------------------------------------------------------------------------

newtype PlanID' a = PlanID {getPlanId :: a}
  deriving newtype (Eq, Show, Read)
  deriving (Functor)

$(makeAdaptorAndInstanceInferrable "pPlanID" ''PlanID')

type PlanIDField = PlanID' (Field SqlUuid)
type PlanIDWrite = PlanID' (Maybe (Field SqlUuid))
type PlanID = PlanID' UUID

deriving via UUID instance (FromHttpApiData PlanID)
deriving via UUID instance (ToJSON PlanID)
deriving via UUID instance (FromJSON PlanID)
instance ToJWT PlanID
instance FromJWT PlanID

---------------------------------------------------------------------------------

data Plan' pid psource pquery puser = Plan
  { planID :: pid
  , planSource :: psource
  , planQuery :: pquery
  , planUserId :: puser
  }

type Plan =
  Entity
    ( Plan'
        PlanID
        NonEmptyText
        (Maybe NonEmptyText)
        (Maybe UserID)
    )

type PlanWrite =
  EntityWriteField
    ( Plan'
        PlanIDWrite
        (Field SqlText)
        (FieldNullable SqlText)
        (UserID' (FieldNullable SqlUuid))
    )

type PlanField =
  EntityField
    ( Plan'
        PlanIDField
        (Field SqlText)
        (FieldNullable SqlText)
        (UserID' (FieldNullable SqlUuid))
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
        , planUserId = pUserID (UserID $ tableField "user_id")
        }

---------------------------------------------------------------------------------

-- QUERIES

newPlan :: NonEmptyText -> Maybe NonEmptyText -> Maybe UserID -> Insert [(PlanID, Text, Maybe Text)]
newPlan source query mUserId =
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
      -- FIXME: this is literally so stupid
      (UserID $ maybeToNullable $ toFields . getUserId <$> mUserId)

planByID :: PlanID -> Select (PlanIDField, Field SqlText, FieldNullable SqlText, Field SqlTimestamptz)
planByID pid_ = do
  Entity{record, recordCreatedAt} <- selectTable planTable
  where_ $ planID record .=== toFields pid_
  pure (planID record, planSource record, planQuery record, recordCreatedAt)

unclaimedPlansWithIds :: [PlanID] -> PlanField -> Field SqlBool
unclaimedPlansWithIds planIds Entity{record} =
  isNull (getUserId $ planUserId record) .&& in_ (map (toFields . getPlanId) planIds) (getPlanId $ planID record)

claimPlans :: UserID -> [PlanID] -> Update [PlanID]
claimPlans (UserID uid) planIds =
  Update
    { uTable = planTable
    , uUpdateWith = updateRecordWith claimPlan
    , uWhere = unclaimedPlansWithIds planIds
    , uReturning = rReturning (\Entity{record} -> planID record)
    }
  where
    claimPlan planEntity =
      planEntity {
        planID = Just <$> planID planEntity,
        planUserId = UserID $ maybeToNullable $ toFields $ Just uid
      }
