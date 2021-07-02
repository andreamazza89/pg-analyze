{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable
import qualified Data.Yaml as Yaml
import qualified Database.HDBC.PostgreSQL as Db
import qualified Env
import qualified  Explain 
import qualified Initialise

main :: IO ()
main = do
  putStrLn "running explain tests..."
  conn <- Db.connectPostgreSQL ("postgresql://localhost/" <> "test_db")
  explains <- Explain.analyze conn -- wanna extract the 'writing to file' outside of Explain.
  traverse_ saveExplained explains
  putStr $ Explain.viewTable explains
--  Initialise.initialise

saveExplained :: Explain.Explain -> IO ()
saveExplained explain =
  Yaml.encodeFile (Env.explainedDirectory Env.def <> Explain.testName explain) (Explain.raw explain)

-- gotta create a dump with `pg_dump --inserts -c <db_name> > dump.sql`
-- gotta create the test db `psql -c 'create database test_db'` and run the dump once `psql test_db < dump.sql`
-- tables and indexes in queries must be fully qualified (i.e. select * from public.employees)
