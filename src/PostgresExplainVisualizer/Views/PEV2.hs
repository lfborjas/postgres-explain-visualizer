{-# LANGUAGE QuasiQuotes #-}

-- |
module PostgresExplainVisualizer.Views.PEV2 where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid
import PyF (fmt)

page :: Text -> Maybe Text -> UTCTime -> Html ()
page pSource mQuery createdAt = do
  let pQuery =
        case mQuery of
          Nothing -> "`null`" :: Text
          Just q -> mconcat ["`", q, "`"]
  main_ $ do
    planNavbar $ do
      span_ [class_ "navbar-text"] $ do
        time_ [datetime_ (T.pack . iso8601Show $ createdAt)] $ do
          "Plan Created At "
          toHtml $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" createdAt

    noscript_ "Need JS enabled for plan visualization"
    div_ [id_ "app"] mempty
    script_ $ do
      [fmt|
          window.plan = `{pSource}`;
          window.query = {pQuery};
      |]
    -- NOTE: these files are produced by the `npm run build` task within
    -- `visualizer-component`, which is a very tiny Vue project; please run that
    -- task if you update the component
    link_ [href_ "/static/pev2/css/app.css", rel_ "stylesheet"]
    script_ [src_ "/static/pev2/js/app.js"] ("" :: Text)
    script_ [src_ "/static/pev2/js/chunk-vendors.js"] ("" :: Text)

planNotFound :: Html ()
planNotFound = do
  main_ [class_ "container mt-2"] $ do
    planNavbar mempty
    div_ [class_ "alert alert-danger", role_ "alert"] "Plan Not Found"

planNavbar :: Html () -> Html ()
planNavbar titleContent = do
  -- ref: https://getbootstrap.com/docs/4.6/components/navbar/
  nav_ [class_ "navbar navbar-expand-lg navbar-light bg-light justify-content-between"] $ do
    a_ [class_ "navbar-brand", href_ "/"] "PG Explain Visualizer"
    titleContent
    ul_ [class_ "navbar-nav"] $ do
      li_ [class_ "nav-item active"] $ do
        a_ [class_ "btn btn-primary", href_ "/"] "New Plan"
