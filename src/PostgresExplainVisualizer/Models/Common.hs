{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module PostgresExplainVisualizer.Models.Common where

import Data.Profunctor (Profunctor (lmap))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye (
  Field,
  SqlTimestamptz,
  TableFields,
  optionalTableField, ToFields, Column, SqlText, DefaultFromField (defaultFromField), toToFields, sqlStrictText
 )
import PostgresExplainVisualizer.Database.Orphanage ()
import Servant (FromHttpApiData)
import Data.Profunctor.Product.Default
import Data.Text (Text, null)
import Servant.API (FromHttpApiData(parseUrlPiece))

--- ENTITY ABSTRACTION
--- https://williamyaoh.com/posts/2019-12-28-abstracting-out-common-columns-opaleye.html
-- https://github.com/dandoh/web-haskell-graphql-postgres-boilerplate/blob/e673e9ee07ce7a4dd9b023328038664e8fdfdd78/src/Database/Base.hs

data EntityT r c u = Entity
  { record :: r
  , recordCreatedAt :: c
  , recordUpdatedAt :: u
  }

$(makeAdaptorAndInstance "pEntity" ''EntityT)

type Entity a =
  EntityT
    a
    UTCTime
    UTCTime

type EntityWriteField a =
  EntityT
    a
    (Maybe (Field SqlTimestamptz))
    ()

type EntityField a =
  EntityT
    a
    (Field SqlTimestamptz)
    (Field SqlTimestamptz)

withTimestampFields ::
  a ->
  EntityT
    a
    -- created_at: written as maybe, read as Field TsTZ
    (TableFields (Maybe (Field SqlTimestamptz)) (Field SqlTimestamptz))
    -- updated_at: written as unit (readonly), read as field tstz
    (TableFields () (Field SqlTimestamptz))
withTimestampFields mapping =
  Entity
    { record = mapping
    , recordCreatedAt = optionalTableField "created_at"
    , recordUpdatedAt = lmap (\() -> Nothing) (optionalTableField "updated_at") --readOnlyTableField "updated_at"
    }

withTimestamp :: [row] -> [EntityT row (Maybe timestamp) ()]
withTimestamp =
  map f
 where
  f r = Entity{record = r, recordCreatedAt = Nothing, recordUpdatedAt = ()}

-- | Update a given entity by applying the given updater
-- function to the record. Will handle timestamps appropriately.
updateRecordWith ::
  (recordR -> recordW) ->
  -- | read representation of an entity
  EntityT recordR t t ->
  -- | write representation of an entity; createdAt is optional,
  -- updatedAt is readonly.
  EntityT recordW (Maybe t) ()
updateRecordWith f e@Entity{record, recordCreatedAt} =
  e
    { record = f record
    , recordCreatedAt = Just recordCreatedAt
    , recordUpdatedAt = ()
    }

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
