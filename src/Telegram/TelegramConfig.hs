{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}



module Telegram.TelegramConfig where


import Data.Aeson
import GHC.Generics
import Bot.Bot
import Telegram.RequestTelegram


data TelegramConfig = TelegramConfig
  { token :: String
  , help :: String
  , repeats :: Int
--   , logConfig :: LogConfig
  } deriving (Show, Generic)

instance FromJSON TelegramConfig

instance ToJSON TelegramConfig

data TelegramBot = TelegramBot
  { config :: TelegramConfig
  , botUrl :: String
  , getUpdates :: String
  , sendMessage :: String
  , lastMessId :: Integer
  , waitingForRepeats :: Bool
  }

instance Bot TelegramBot where
    -- type BotConfig TelegramBot = c| c -> b
    -- type BotMessage TelegramBot = m| m -> b
    -- getBotWithConfig :: BotConfig b -> b
  type BotConfig  TelegramBot = TelegramConfig
  type BotMessage TelegramBot = TelegramMessage
  
    -- getLastMessage = do
    --     tBot@TelegramBot {config = c, getUpdates = updStr, lastMessId = oldId} <-
    --       get
    --     let lc = logConfig c
    --         logL = logLevel lc
    --         logF = logFile lc
    --     upd <- liftIO $ catch (simpleHttp updStr) (handleGetException lc)
    --     let updates = eitherDecode upd :: Either String Updates
    --     let msg = processUpdates oldId updates
    --     put $ maybe tBot (\m -> tBot {lastMessId = message_id m}) msg
    --     return msg
    --   sendMessageTo mChat txt = do
    --     b@TelegramBot {config = c, sendMessage = sendUrl, waitingForRepeats = wr} <-
    --       get
    --     let lc = logConfig c
    --     liftIO $ sendText lc txt (chat_id mChat) sendUrl