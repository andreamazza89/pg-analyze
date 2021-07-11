{-# LANGUAGE OverloadedStrings #-}

module Main where

--import qualified Initialise

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Foldable
import qualified Data.Yaml as Yaml
import qualified Database.HDBC.PostgreSQL as Db
import qualified Env
import qualified Explain

main :: IO ()
main = do
  putStrLn "running explain tests..."
  environment <- Env.def <$> Db.connectPostgreSQL ("postgresql://localhost/" <> "test_db")
  explains <- runReaderT Explain.analyze environment
  saveExplained explains environment
  putStr $ Explain.viewTable explains
  
--  runReaderT Initialise.initialise environment

saveExplained :: [Explain.Explain] -> Env.Env -> IO ()
saveExplained explains =
  runReaderT (traverse_ save explains)
  where
    save explain = do
      explainedDirectory <- asks Env.explainedDirectory
      liftIO $ Yaml.encodeFile (explainedDirectory <> Explain.testName explain) (Explain.raw explain)

-- gotta create a dump with `pg_dump --inserts -c <db_name> > dump.sql`
-- gotta create the test db `psql -c 'create database test_db'` and run the dump once `psql test_db < dump.sql`
-- tables and indexes in queries must be fully qualified (i.e. select * from public.employees)
