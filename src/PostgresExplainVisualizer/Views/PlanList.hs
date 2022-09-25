{-# LANGUAGE QuasiQuotes #-}
-- |

module PostgresExplainVisualizer.Views.PlanList where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid
import PyF (fmt)
import PostgresExplainVisualizer.Server.Types (UserSessionData(..), PlanView)
import PostgresExplainVisualizer.Models.Plan (PlanID, PlanID' (getPlanId))
import Data.UUID (toText)

page :: [PlanView] -> Html ()
page = mempty
