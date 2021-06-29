{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson as JSON
import Data.Foldable
import qualified Data.Yaml as Yaml
import qualified Database.HDBC as HDb
import qualified Database.HDBC.PostgreSQL as HDb
import System.Directory as Directory

data ToRun = ToRun
  { setup :: Maybe String,
    testQuery :: String,
    name :: String
  }
  deriving (Show)

data TestConfig = TestConfig
  { setup' :: Maybe String,
    testQuery' :: String
  }
  deriving (Show)

instance FromJSON TestConfig where
  parseJSON (Yaml.Object o) =
    TestConfig
      <$> o .:? "setup"
      <*> o .: "query"
  parseJSON _ = fail "config must be an object"

main :: IO ()
main = do
  toRun <- loadAnalyses
  conn <- HDb.connectPostgreSQL ("postgresql://localhost/" <> "test_db")
  runAnalyses toRun conn
  print ("done" :: String)

loadAnalyses :: IO [ToRun]
loadAnalyses = do
  files <- Directory.listDirectory "./tests"
  traverse (\fname -> toToRun fname <$> Yaml.decodeFileThrow ("./tests/" <> fname)) files

toToRun :: FilePath -> TestConfig -> ToRun
toToRun fname (TestConfig s q) = ToRun s q fname

runAnalyses :: [ToRun] -> HDb.Connection -> IO ()
runAnalyses tests conn =
  mapM_ (runAnalysis conn) tests

runAnalysis :: HDb.Connection -> ToRun -> IO ()
runAnalysis conn toRun = do
  prepareDatabase conn
  [[explainOutput]] <- runTest conn toRun
  writeFile ("./explained/" <> name toRun) (HDb.fromSql explainOutput)

prepareDatabase :: HDb.Connection -> IO ()
prepareDatabase conn =
  readFile "./dump.sql" >>= HDb.runRaw conn

runTest :: HDb.Connection -> ToRun -> IO [[HDb.SqlValue]]
runTest conn (ToRun s q _) = do
  traverse_ (HDb.runRaw conn) s
  HDb.quickQuery conn (explained q) []

explained :: String -> String
explained q =
  "EXPLAIN (ANALYZE TRUE, FORMAT YAML) (" <> q <> ");"

-- paths are relative, so expecting a ./tests/ directory with the queries, and and ./explained/ for output + dump should be named dump.sql
-- gotta create a dump with `pg_dump --inserts -c <db_name> > dump.sql`
-- gotta create the test db `psql -c 'create database test_db'` and run the dump once `psql test_db < dump.sql`
-- tables and indexes in queries must be fully qualified (i.e. select * from public.employees)
