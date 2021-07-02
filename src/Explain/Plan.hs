module Explain.Plan (load, setup, query, name, Plan) where

import qualified Data.Yaml as Yaml
import qualified Env
import qualified Explain.Config as Explain
import System.Directory as Directory

data Plan = Plan
  { setup :: Maybe DbQuery,
    query :: DbQuery,
    name :: String
  }
  deriving (Show)

type DbQuery = String

load :: IO [Plan]
load =
  listConfigs >>= traverse parseConfig
  where
    listConfigs :: IO [FilePath]
    listConfigs = Directory.listDirectory testsDirectory

    parseConfig :: FilePath -> IO Plan
    parseConfig filePath =
      fromConfig filePath
        <$> Yaml.decodeFileThrow (testsDirectory <> filePath)

    fromConfig :: FilePath -> Explain.Config -> Plan
    fromConfig filePath (Explain.Config s q) =
      Plan s q filePath

    testsDirectory = Env.testsDirectory Env.def
