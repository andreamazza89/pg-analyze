{-# LANGUAGE OverloadedStrings #-}

module Explain.Config where

import Data.Aeson as JSON

data Config = Config
  { setup :: Maybe String,
    query :: String
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON (JSON.Object o) =
    Config
      <$> o .:? "setup"
      <*> o .:  "query"
  parseJSON _ = fail "config must be an object"
