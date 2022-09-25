-- |

module PostgresExplainVisualizer.Server.Types where
import Data.Text ( Text )
import PostgresExplainVisualizer.Models.User ( UserID )
import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON, FromJSON )
import Servant.Auth.Server ( FromJWT, ToJWT )
import PostgresExplainVisualizer.Models.Plan (PlanID)
import PostgresExplainVisualizer.Client.Github.Api (OAuthState)

data UserSessionData = UserSessionData
  { username :: Text
  , currentUserId :: UserID
  } deriving (Eq, Show, Read, Generic)

instance ToJSON UserSessionData
instance ToJWT UserSessionData
instance FromJSON UserSessionData
instance FromJWT UserSessionData

data AnonymousData = AnonymousData
  { unclaimedPlans :: [PlanID]
  , oAuthNegotiationState :: Maybe OAuthState
  , returnToAfterLogin :: Maybe Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON AnonymousData
instance ToJWT AnonymousData
instance FromJSON AnonymousData
instance FromJWT AnonymousData

data Session
  = Authenticated UserSessionData
  | Anonymous AnonymousData
  deriving (Eq, Show, Read, Generic)

instance ToJSON Session
instance ToJWT Session
instance FromJSON Session
instance FromJWT Session
