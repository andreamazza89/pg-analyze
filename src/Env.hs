module Env
  ( build,
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

build :: Db.Connection -> Env
build conn =
  Env
    conn
    "./tests/"
    "./dump.sql"
    "./explained/"
    "test_db"
