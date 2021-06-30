module Explain (analyze) where

import Data.Foldable
import qualified Data.Yaml as Yaml
import qualified Database.HDBC as Db
import qualified Database.HDBC.PostgreSQL as Db
import qualified Explain.Config as Explain
import System.Directory as Directory

data ExplainPlan = ExplainPlan
  { setup :: Maybe DbQuery,
    query :: DbQuery,
    name :: String
  }
  deriving (Show)

type DbQuery = String

analyze :: Db.Connection -> IO ()
analyze conn =
  loadAnalyses
    >>= runAnalyses conn

loadAnalyses :: IO [ExplainPlan]
loadAnalyses =
  listConfigs
    >>= traverse parseConfig
  where
    listConfigs :: IO [FilePath]
    listConfigs = Directory.listDirectory "./tests"

    parseConfig :: FilePath -> IO ExplainPlan
    parseConfig filePath =
      fromConfig filePath
        <$> Yaml.decodeFileThrow ("./tests/" <> filePath)

    fromConfig :: FilePath -> Explain.Config -> ExplainPlan
    fromConfig filePath (Explain.Config s q) =
      ExplainPlan s q filePath

runAnalyses :: Db.Connection -> [ExplainPlan] -> IO ()
runAnalyses conn = mapM_ (runAnalysis conn)

runAnalysis :: Db.Connection -> ExplainPlan -> IO ()
runAnalysis conn toRun =
  resetDatabase conn
    >> runExplain conn toRun
    >>= writeFile ("./explained/" <> name toRun)

resetDatabase :: Db.Connection -> IO ()
resetDatabase conn =
  readFile "./dump.sql" >>= Db.runRaw conn

runExplain :: Db.Connection -> ExplainPlan -> IO String
runExplain conn (ExplainPlan s q _) =
  prepare
    >> explainQuery
  where
    prepare = traverse_ (Db.runRaw conn) s
    explainQuery = Db.fromSql . firstColumn . firstRow <$> Db.quickQuery conn (explained q) []

firstRow :: [a] -> a
firstRow = head

firstColumn :: [a] -> a
firstColumn = head

explained :: String -> String
explained q =
  "EXPLAIN (ANALYZE TRUE, FORMAT YAML, VERBOSE TRUE) (" <> q <> ");"
