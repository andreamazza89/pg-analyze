{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Explain
  ( analyze,
    viewTable,
    testName,
    raw,
    Explain,
  )
where

import Colonnade
import Data.Aeson as JSON
import Data.Foldable
import Data.Vector ((!))
import qualified Data.Yaml as Yaml
import qualified Database.HDBC as Db
import qualified Database.HDBC.PostgreSQL as Db
import qualified Explain.Plan as Explain
import qualified Explain.Plan as Plan
import qualified Env

data Explain = Explain
  { totalCost :: Double,
    testName :: String,
    raw :: JSON.Object
  }
  deriving (Show)

analyze :: Db.Connection -> IO [Explain]
analyze conn =
  Plan.load >>= runAnalyses conn

runAnalyses :: Db.Connection -> [Explain.Plan] -> IO [Explain]
runAnalyses conn = traverse (runAnalysis conn)

runAnalysis :: Db.Connection -> Explain.Plan -> IO Explain
runAnalysis conn toRun = do
  resetDatabase conn
  runExplain conn toRun

resetDatabase :: Db.Connection -> IO ()
resetDatabase conn =
  readFile (Env.sqlDump Env.def) >>= Db.runRaw conn

runExplain :: Db.Connection -> Explain.Plan -> IO Explain
runExplain conn plan =
  prepare >> explainQuery
  where
    prepare = traverse_ (Db.runRaw conn) (Plan.setup plan)
    explainQuery = fromDb (Plan.name plan) =<< Db.quickQuery conn (explained $ Plan.query plan) []

explained :: String -> String
explained q =
  "EXPLAIN (ANALYZE TRUE, FORMAT YAML, VERBOSE TRUE) (" <> q <> ");"

data ExplainIntermediate = ExplainIntermediate
  { totalCostIntermediate :: Double,
    rawIntermediate :: JSON.Object
  }

instance JSON.FromJSON ExplainIntermediate where
  parseJSON (JSON.Array arr) =
    case firstRowFirstColumn arr of
      Just explainOutput ->
        ExplainIntermediate <$> ((explainOutput .: "Plan") >>= (.: "Total Cost")) <*> pure explainOutput
      Nothing -> fail "boom"
    where
      firstRowFirstColumn :: JSON.Array -> Maybe JSON.Object
      firstRowFirstColumn array = case array ! 0 of
        Object o -> Just o
        _ -> Nothing
  parseJSON _ = fail "explain output must be an array"

fromDb :: String -> [[Db.SqlValue]] -> IO Explain
fromDb name' [[explainOutput]] =
  toExplain' <$> Yaml.decodeThrow (Db.fromSql explainOutput)
  where
    toExplain' :: ExplainIntermediate -> Explain
    toExplain' intermediate = Explain (totalCostIntermediate intermediate) name' (rawIntermediate intermediate)
fromDb _ _ =
  fail "explain output should be one column / one row"

-- Showing explain output as a table (should probs move it away from this module)

format :: Colonnade Headed Explain String
format =
  mconcat
    [ headed "Test name" testName,
      headed "Total cost" (show . totalCost)
    ]

viewTable :: Foldable t => t Explain -> String
viewTable =
  ascii format
