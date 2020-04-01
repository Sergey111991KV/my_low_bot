{-# LANGUAGE DeriveGeneric #-}

module Logging.Logging where

import Data.Aeson
import GHC.Generics

data Logging
  = Debug
  | Warning
  | ErrorS
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON Logging

instance FromJSON Logging

data LogConfig = LogConfig
  { logFile :: FilePath
  , logLevel :: Logging
  , logConsole :: Bool
  } deriving (Show, Generic)

instance ToJSON LogConfig

instance FromJSON LogConfig
