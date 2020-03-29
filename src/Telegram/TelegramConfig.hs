{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.TelegramConfig where


import Data.Aeson
import GHC.Generics
-- import Logging.Config

data TelegramConfig = TelegramConfig
  { token :: String
  , help :: String
  , repeats :: Int
--   , logConfig :: LogConfig
  } deriving (Show, Generic)

-- instance FromJSON TelegramConfig

-- instance ToJSON TelegramConfig

data TelegramBot = TelegramBot
  { config :: TelegramConfig
  , botUrl :: String
  , getUpdates :: String
  , sendMessage :: String
  , lastMessId :: Integer
  , waitingForRepeats :: Bool
  }

-- runTelegramEcho :: TelegramConfig -> IO ()
-- runTelegramEcho = startEchoBot

-- getTelegramConfig :: IO TelegramConfig
-- getTelegramConfig = do
--   telegramToken   <- fromMaybe undefined <$> lookupEnv "1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo" -- "TG_TOKEN"
--   telegramRepeats <- maybe 3 read <$> lookupEnv "TG_REPEATS"
--   telegramHelp <- fromMaybe ("Echo bot. Repeats every message n times (default n = " ++ show telegramRepeats ++ "). To change n write /repeat") <$> lookupEnv "TG_HELP"
--   return $ TelegramConfig telegramToken telegramHelp telegramRepeats

-- instance EchoBot TelegramBot where
--     helpMessage