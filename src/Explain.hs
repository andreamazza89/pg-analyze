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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Aeson as JSON
import Data.Foldable
import Data.Vector ((!))
import qualified Data.Yaml as Yaml
import qualified Database.HDBC as Db
import qualified Database.HDBC.PostgreSQL as Db
import qualified Env
import qualified Explain.Plan as Explain
import qualified Explain.Plan as Plan
import Types

data Explain = Explain
  { totalCost :: Double,
    testName :: String,
    raw :: JSON.Object
  }
  deriving (Show)

analyze :: App [Explain]
analyze = do
  Plan.load >>= traverse runAnalysis

runAnalysis :: Explain.Plan -> App Explain
runAnalysis plan = do
  conn <- asks Env.dbConnection
  sqlDump <- asks Env.sqlDump
  liftIO $ resetDatabase conn sqlDump
  liftIO $ runExplain conn plan

resetDatabase :: Db.Connection -> FilePath -> IO ()
resetDatabase conn sqlDump = do
  readFile sqlDump >>= Db.runRaw conn

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
fromDb planName [[explainOutput]] =
  fromDb' <$> Yaml.decodeThrow (Db.fromSql explainOutput)
  where
    fromDb' :: ExplainIntermediate -> Explain
    fromDb' intermediate = Explain (totalCostIntermediate intermediate) planName (rawIntermediate intermediate)
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
