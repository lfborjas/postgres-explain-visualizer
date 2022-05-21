-- |
module PostgresExplainVisualizer.Views.NewPlan where

import Control.Monad (when)
import Data.Maybe
import Data.Text
import Lucid

page :: Maybe Text -> Html ()
page err = do
  let wasValidated =
        if isJust err
          then "was-validated"
          else ""
  main_ [class_ "container mt-2"] $ do
    h1_ "Postgres Explain Visualizer"
    palaver
    -- ref: https://getbootstrap.com/docs/4.6/components/forms/
    form_ [action_ "/plans", method_ "post", class_ wasValidated] $ do
      errorDisplay err
      div_ [class_ "form-group"] $ do
        label_ [for_ "plan"] "Plan"
        textarea_ [class_ "form-control", id_ "plan", name_ "plan", rows_ "10", required_ ""] ""
      div_ [class_ "form-group"] $ do
        label_ [for_ "query"] "Query (optional)"
        textarea_ [class_ "form-control", id_ "query", name_ "query", rows_ "10"] ""
      button_ [type_ "submit", class_ "btn btn-primary"] "Submit"

errorDisplay :: Maybe Text -> Html ()
errorDisplay err = do
  when (isJust err) $ do
    div_ [class_ "bg-error"] $ do
      toHtml . fromJust $ err

palaver :: Html ()
palaver = do
  p_ [class_ "lead"] $ do
    "Paste the output of "
    code_ "EXPLAIN (ANALYZE,BUFFERS)"
    " in the "
    code_ "Plan"
    " field. Optionally, provide the original query. "
  p_ $ do
    "When you hit "
    code_ "Submit"
    " Your plan "
    strong_ "will be stored in the backend "
    "so you can have a permalink to share with colleagues or put in documentation. "
  p_ $ do
    "There is no public archive, but consider "
    a_ [href_ "https://github.com/lfborjas/postgres-explain-visualizer"] "hosting your own copy "
    "or using either "
    a_ [href_ "https://explain.dalibo.com/"] "a non-persistent alternative "
    "or "
    a_ [href_ "https://explain.depesz.com/"] "an optionally obfuscating alternative "
    "if you're concerned about security."
