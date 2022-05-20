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
    script_ [src_ "https://unpkg.com/vue@3"] (""::Text)
    script_ [type_ "module", src_ "/static/main.js"] (""::Text)
    script_ $ do
      [fmt|
          window.plan = `{pSource}`;
          window.query = {pQuery};
      |]
