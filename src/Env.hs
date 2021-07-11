module Env
  ( def,
    Env (..),
  )
where

import qualified Database.HDBC.PostgreSQL as Db

data Env = Env
  { dbConnection :: Db.Connection,
    testsDirectory :: FilePath,
    sqlDump :: FilePath,
    explainedDirectory :: FilePath,
    testDb :: String
  }

def :: Db.Connection -> Env
def conn =
  Env
    conn
    "./tests/"
    "./dump.sql"
    "./explained/"
    "test_db"
