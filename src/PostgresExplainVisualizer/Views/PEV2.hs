{-# LANGUAGE QuasiQuotes #-}
-- |

module PostgresExplainVisualizer.Views.PEV2 where
import Data.Text
import Lucid
import PyF

page :: Text -> Maybe Text -> Html ()
page pSource mQuery = do
  let pQuery =
        case mQuery of
          Nothing -> "`null`" :: Text
          Just q -> mconcat ["`", q, "`"]
  main_ $ do
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
    script_ [src_ "/static/pev2/js/app.js"] (""::Text)
    script_ [src_ "/static/pev2/js/chunk-vendors.js"] (""::Text)
