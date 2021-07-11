module Explain.Plan
  ( load,
    Plan (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Data.Yaml as Yaml
import qualified Env
import qualified Explain.Config as Explain
import System.Directory as Directory
import Types

data Plan = Plan
  { setup :: Maybe DbQuery,
    query :: DbQuery,
    name :: String
  }
  deriving (Show)

type DbQuery = String

load :: App [Plan]
load =
  asks Env.testsDirectory
    >>= listConfigs
    >>= traverse parseConfig
  where
    listConfigs :: FilePath -> App [FilePath]
    listConfigs = liftIO . Directory.listDirectory

    parseConfig :: FilePath -> App Plan
    parseConfig filePath = do
      testsDirectory <- asks Env.testsDirectory
      fromConfig filePath
        <$> Yaml.decodeFileThrow (testsDirectory <> filePath)

    fromConfig :: FilePath -> Explain.Config -> Plan
    fromConfig filePath (Explain.Config s q) =
      Plan s q filePath
