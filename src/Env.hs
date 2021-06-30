module Env (Env (..), def) where

data Env = Env
  { testsDirectory :: FilePath,
    explainedDirectory :: FilePath,
    sqlDump :: FilePath,
    testDb :: String
  }

def :: Env
def =
  Env
    "./tests/"
    "./explained/"
    "./dump.sql"
    "test_db"
