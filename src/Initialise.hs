module Initialise (initialise) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Function ((&))
import qualified Data.List as List
import qualified Env
import qualified System.Process as Proc
import Types

initialise :: App ()
initialise = do
  env <- ask
  liftIO $ putStrLn "initialising the testing environment"
  _ <- liftIO $ Proc.createProcess $ Proc.shell (script env)
  liftIO $ putStrLn "init done"
  where
    -- message to explain how the thing works, saying to replace the sample dump with one created with pg_dump.... and also to run it a first time using psql test_db < dump.sql

    script env =
      List.intercalate
        " && "
        [ testsDirectory env,
          explainedDirectory env,
          sampleTest env,
          sampleDump env,
          dropTestDb env & silence,
          createTestDb env & silence,
          hydrateDb env & silence
        ]

    testsDirectory env =
      "mkdir -p " <> Env.testsDirectory env
    explainedDirectory env =
      "mkdir -p " <> Env.explainedDirectory env
    sampleTest env =
      "echo " <> sampleTestContent <> " > " <> Env.testsDirectory env <> "sampleTest.yml"
    sampleTestContent =
      "'setup: >\n  CREATE INDEX pet_age_idx ON pets (age);\nquery: >\n  SELECT * from public.pets\n  WHERE age = 33'"
    sampleDump env =
      "echo " <> sampleDumpContent <> " > " <> Env.sqlDump env
    sampleDumpContent =
      "'DROP TABLE public.pets;\nCREATE TABLE public.pets (name varchar PRIMARY KEY, age int NOT NULL);'"
    dropTestDb env =
      "psql -c 'DROP DATABASE IF EXISTS " <> Env.testDb env <> ";'"
    createTestDb env =
      "psql -c 'CREATE DATABASE " <> Env.testDb env <> ";'"
    hydrateDb env =
      "psql " <> Env.testDb env <> " < " <> Env.sqlDump env
    silence command =
      command <> " &> /dev/null"
