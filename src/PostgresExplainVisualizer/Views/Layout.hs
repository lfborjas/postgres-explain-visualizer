module PostgresExplainVisualizer.Views.Layout where

import Lucid
import Data.Text qualified as T

header :: Html ()
header = do
  head_ $ do
    title_ "Postgres Explain Visualizer"
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "description", content_ "PEV"]
    favicon
    stylesheets

layout :: Html () -> Html ()
layout page = do
  doctype_
  html_ [lang_ "en"] $ do
    header
    body_ $ do
      div_ [class_ "container-fluid"] $ do
        page
        -- TODO: maybe add a footer?

stylesheets :: Html ()
stylesheets = do
  -- REVIEW: should we also serve these?
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/bootstrap@4.5.0/dist/css/bootstrap.min.css "]
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/@fortawesome/fontawesome-free@5.13.0/css/all.css "]

favicon :: Html ()
favicon = do
    -- from https://favicon.io/emoji-favicons/thinking-face
    link_ [rel_ "apple-touch-icon", sizes_ "180x180", assetRef' "apple-touch-icon.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", assetRef' "favicon-32x32.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", assetRef' "favicon-16x16.png"]
    link_ [rel_ "manifest", assetRef' "site.webmanifest"]
    where
        assetRef' = href_ . T.pack . mappend "/static/"
