{-# LANGUAGE ImportQualifiedPost #-}
-- |

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
      div_ [class_ "container grid-lg"] $ do
        header_ [class_ "navbar mt-2"] $ do
          section_ [class_ "navbar-section"] $ do
            a_ [href_ "/", class_ "navbar-brand mr-2"] "New Explain"
        page

stylesheets :: Html ()
stylesheets = do
  link_ [rel_ "stylesheet", href_ "/static/css/spectre.min.css"]
  link_ [rel_ "stylesheet", href_ "/static/css/spectre-exp.min.css"]
  link_ [rel_ "stylesheet", href_ "/static/css/spectre-icons.min.css"]

favicon :: Html ()
favicon = do
    -- from https://favicon.io/emoji-favicons/thinking-face
    link_ [rel_ "apple-touch-icon", sizes_ "180x180", assetRef' "apple-touch-icon.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", assetRef' "favicon-32x32.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", assetRef' "favicon-16x16.png"]
    link_ [rel_ "manifest", assetRef' "site.webmanifest"]
    where
        assetRef' = href_ . T.pack . mappend "/static/"
