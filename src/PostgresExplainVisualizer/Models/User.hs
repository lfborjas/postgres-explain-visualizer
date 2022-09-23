{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- |

module PostgresExplainVisualizer.Models.User where

import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.UUID (UUID)
import Opaleye (
  Field,
  Insert (..),
  OnConflict (DoNothing),
  Select,
  SqlText,
  SqlUuid,
  Table,
  optionalTableField,
  rReturning,
  requiredTableField,
  selectTable,
  table,
  toFields,
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
import Data.Aeson (ToJSON, FromJSON)
import Servant.Auth.Server (ToJWT, FromJWT)

---------------------------------------------------------------------------------

newtype UserID' a = UserID {getUserId :: a}
  deriving newtype (Eq, Show, Read)
  deriving (Functor)

$(makeAdaptorAndInstanceInferrable "pUserID" ''UserID')

type UserIDField = UserID' (Field SqlUuid)
type UserIDWrite = UserID' (Maybe (Field SqlUuid))
type UserID = UserID' UUID

deriving via UUID instance (FromHttpApiData UserID)
deriving via UUID instance (ToJSON UserID)
deriving via UUID instance (FromJSON UserID)
instance ToJWT UserID
instance FromJWT UserID
---------------------------------------------------------------------------------

data User' pid pusername = User
  { userID :: pid
  , githubUsername :: pusername
  }

type User =
  Entity
    ( User'
        UserID
        NonEmptyText
    )

type UserWrite =
  EntityWriteField
    ( User'
        UserIDWrite
        (Field SqlText)
    )

type UserField =
  EntityField
    ( User'
        UserIDField
        (Field SqlText)
    )

$(makeAdaptorAndInstanceInferrable "pUser" ''User')

---------------------------------------------------------------------------------

userTable ::
  Table UserWrite UserField
userTable =
  table "user_account" . pEntity . withTimestampFields $
    pUser
      User
        { userID = pUserID (UserID $ optionalTableField "id")
        , githubUsername = requiredTableField "github_username"
        }

---------------------------------------------------------------------------------

-- QUERIES

newUser :: NonEmptyText -> Insert [(UserID, Text)]
newUser uname =
  Insert
    { iTable = userTable
    , iRows = withTimestamp [row]
    , iReturning = rReturning (\Entity{record} -> (userID record, githubUsername record))
    , iOnConflict = Just DoNothing
    }
  where
    row =
      User
        (UserID Nothing)
        (toFields uname)

userByGithubUsername :: NonEmptyText -> Select (UserIDField, Field SqlText)
userByGithubUsername uname = do
  Entity{record} <- selectTable userTable
  where_ $ githubUsername record .=== toFields uname
  pure (userID record, githubUsername record)
