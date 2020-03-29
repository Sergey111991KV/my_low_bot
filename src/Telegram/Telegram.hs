{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Telegram where

import Data.Maybe
import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy as B
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import Network.HTTP.Conduit
import Data.Aeson
import GHC.Generics
import System.Environment
import Text.Read (readMaybe)


import Telegram.RequestTelegram
import Telegram.TelegramConfig

runTelegramEcho :: TelegramConfig -> IO ()
runTelegramEcho = moveRequest


getTelegramConfig :: IO TelegramConfig
getTelegramConfig = do
        let telegramToken = "1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo"
        telegramRepeats <- maybe 3 read <$> lookupEnv "TG_REPEATS"
        telegramHelp    <- fromMaybe ("Echo bot. Repeats every message n times (default n = " ++ show telegramRepeats ++ "). To change n write /repeat") <$> lookupEnv "TG_HELP"
        return $ TelegramConfig telegramToken telegramHelp telegramRepeats

getBotTelegramWithConfig :: TelegramConfig -> TelegramBot
getBotTelegramWithConfig c =
        let bUrl = "https://api.telegram.org/bot" ++ token c ++ "/"
                         in TelegramBot
                              c
                              bUrl
                              (bUrl ++ "getUpdates")
                              (bUrl ++ "sendMessage")
                              0
                              False



moveRequest :: TelegramConfig-> IO ()
moveRequest c = do
        bot <- return $ getBotTelegramWithConfig c
        request <- parseRequest (getUpdates bot)
        response  <- httpJSON request :: IO (Response Updates)
        let d = (getResponseStatusCode response)
        case d of
              200 -> print (responseBody response)
              _   -> print "( message (last (result ps)))"
        
        
        