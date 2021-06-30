{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Database.HDBC.PostgreSQL as Db
import qualified Explain

main :: IO ()
main = do
  putStrLn "running explain tests..."
  conn <- Db.connectPostgreSQL ("postgresql://localhost/" <> "test_db")
  Explain.analyze conn
  putStrLn "all done"

-- gotta create a dump with `pg_dump --inserts -c <db_name> > dump.sql`
-- gotta create the test db `psql -c 'create database test_db'` and run the dump once `psql test_db < dump.sql`
-- tables and indexes in queries must be fully qualified (i.e. select * from public.employees)
