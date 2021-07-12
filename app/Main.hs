{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Foldable
import qualified Data.Yaml as Yaml
import qualified Database.HDBC.PostgreSQL as Db
import qualified Env
import qualified Explain
import qualified Initialise
import qualified Options.Applicative as Options

data Command = Initialise | Analyze
  deriving (Show)

main :: IO ()
main = do
  Options.execParser commandParser >>= runCommand

commandParser :: Options.ParserInfo Command
commandParser =
  Options.info
    (Options.helper <*> Options.subparser (initialise <> analyze))
    mempty
  where
    initialise =
      Options.command
        "initialise"
        (Options.info (pure Initialise) (Options.progDesc "Initialise a directory for pg-analyze"))

    analyze =
      Options.command
        "analyze"
        (Options.info (pure Analyze) (Options.progDesc "Run the analysis"))

runCommand :: Command -> IO ()
runCommand Analyze = do
  putStrLn "running explain tests..."
  environment <- buildEnvironment
  explains <- runReaderT Explain.analyze environment
  saveExplained explains environment
  putStr $ Explain.viewTable explains
runCommand Initialise = do
  environment <- buildEnvironment
  runReaderT Initialise.initialise environment

buildEnvironment :: IO Env.Env
buildEnvironment =
  Env.build <$> Db.connectPostgreSQL ("postgresql://localhost/" <> "test_db")

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
