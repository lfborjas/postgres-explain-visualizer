{-# LANGUAGE ImportQualifiedPost #-}
-- |

module PostgresExplainVisualizer.Database.Pool where

import Data.Pool
import Database.PostgreSQL.Simple qualified as PGS
import Data.Text
import Data.Text.Encoding (encodeUtf8)

-- Two sources:
-- https://github.com/sboehler/freer-servant/blob/52f9437cc3fddf735180e43a1d9d5a9f4026abbd/src/Capabilities/Database.hs
-- https://docs.servant.dev/en/stable/cookbook/db-postgres-pool/PostgresPool.html
initPool :: Text -> IO (Pool PGS.Connection)
initPool urlTxt =
  createPool
    (PGS.connectPostgreSQL $ encodeUtf8 urlTxt)
    PGS.close
    1 -- stripes
    10 -- keep unused connections open for 10 seconds
    10 -- max 10 connections per stripe
