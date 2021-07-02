module Initialise (initialise) where

import Data.Function ((&))
import qualified Data.List as List
import qualified Env
import qualified System.Process as Proc

initialise :: IO ()
initialise = do
  putStrLn "initialising the testing environment"
  _ <- Proc.createProcess $ Proc.shell script
  putStrLn "init done"
  where
    -- message to explain how the thing works, saying to replace the sample dump with one created with pg_dump.... and also to run it a first time using psql test_db < dump.sql

    script =
      List.intercalate
        " && "
        [ testsDirectory,
          explainedDirectory,
          sampleTest,
          sampleDump,
          dropTestDb & silence,
          createTestDb & silence,
          hydrateDb & silence
        ]

    testsDirectory =
      "mkdir -p " <> Env.testsDirectory Env.def
    explainedDirectory =
      "mkdir -p " <> Env.explainedDirectory Env.def
    sampleTest =
      "echo " <> sampleTestContent <> " > " <> Env.testsDirectory Env.def <> "sampleTest.yml"
    sampleTestContent =
      "'setup: >\n  CREATE INDEX pet_age_idx ON pets (age);\nquery: >\n  SELECT * from public.pets\n  WHERE age = 33'"
    sampleDump =
      "echo " <> sampleDumpContent <> " > " <> Env.sqlDump Env.def
    sampleDumpContent =
      "'DROP TABLE public.pets;\nCREATE TABLE public.pets (name varchar PRIMARY KEY, age int NOT NULL);'"
    dropTestDb =
      "psql -c 'DROP DATABASE IF EXISTS " <> Env.testDb Env.def <> ";'"
    createTestDb =
      "psql -c 'CREATE DATABASE " <> Env.testDb Env.def <> ";'"
    hydrateDb =
      "psql " <> Env.testDb Env.def <> " < " <> Env.sqlDump Env.def
    silence command =
      command <> " &> /dev/null"
