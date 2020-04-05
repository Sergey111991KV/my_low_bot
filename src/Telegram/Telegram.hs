{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}



module Telegram.Telegram where

import Data.Maybe
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple 
import qualified Data.ByteString.Lazy as B
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import Data.Aeson
import GHC.Generics
import System.Environment
import Text.Read (readMaybe)
import Data.Typeable

import Bot.EchoBot
import Telegram.RequestTelegram
import Telegram.TelegramConfig
import Logging.Logging
import Telegram.KeyboardTelegram

runTelegramEcho :: TelegramConfig -> Integer -> IO ()
runTelegramEcho c i = moveRequest c i



getTelegramConfig :: IO TelegramConfig
getTelegramConfig = do
        let telegramToken = "1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo"
        telegramRepeats <- maybe 1 read <$> lookupEnv "TG_REPEATS"
        telegramHelp    <- fromMaybe ("Echo bot. Repeats every message n times (default n = " ++ show telegramRepeats ++ "). To change n write /repeat") <$> lookupEnv "TG_HELP"
        logFile         <- fromMaybe "telegram.log" <$> lookupEnv "TG_LOG_FILE"
        logConsoleStr   <- fromMaybe "False" <$> lookupEnv "TG_LOG_CONSOLE"
        logLevelStr     <- fromMaybe "Debug" <$> lookupEnv "TG_LOG_LEVEL"
        let logLevel   = fromMaybe (error "incorrect log level!") $ readMaybe logLevelStr
            logConsole = fromMaybe (error "incorrect log_console parameter!") $ readMaybe logConsoleStr
            logConfig  = LogConfig logFile logLevel logConsole
        return $ TelegramConfig telegramToken telegramHelp telegramRepeats logConfig

getBotTelegramWithConfig :: TelegramConfig -> Integer -> TelegramBot
getBotTelegramWithConfig c i = 
        let bUrl = "https://api.telegram.org/bot" ++ token c ++ "/"
        in TelegramBot
             c
             bUrl
             (bUrl ++ "getUpdates")
             (bUrl ++ "sendMessage")
             i
             False


moveRequest :: TelegramConfig -> Integer -> IO ()
moveRequest c i = do
        bot       <- return $ getBotTelegramWithConfig c i
        request   <- parseRequest (getUpdates bot)

        response  <- httpJSON request :: IO (Response Updates)

        let d = (getResponseStatusCode response)
        let configLog = logLevel $ logConfig $ config  bot
        let fjleLog   = logFile  $ logConfig $ config  bot
        if d == 200  then do
                if configLog == Debug then
                                logSendFile configLog fjleLog
                else
                        print ""
                let lastMess = findLastMessage (lastMessId bot) (result $ getResponseBody response)
                let idMes = message_id $ message $ last (result $ getResponseBody response)
                if idMes > i then do
                        let textMess = text $ fromJust lastMess
                        case textMess of
                                "/help"    -> do   
                                                let urlSend = urlHelpMessage bot lastMess
                                                requestNew <- parseRequest $ urlSend
                                                responseForMessage <- httpLbs requestNew
                                                print $ helpMessage bot
                                "/repeat" -> do
                                                let idd = chat_id $ chat $ fromJust lastMess
                                                let botur = botUrl bot
                                                keyboardMessageSend idd botur
                                                print idd
                                                print botur


                                _           -> do
                                                let count   = repeats $ config bot
                                                let urlSend = urlLastMessage bot lastMess
                                                repeatMessage count urlSend
                                                threadDelay 10
                                                moveRequest c idMes
                else do
                        print "non repeats message"
                        threadDelay 10
                        moveRequest c idMes
        else do
                 let configl = logConfig c
                 let lC = logConsole configl 
                 let lF = logFile    configl
                 if lC then
                        appendFile lF (show response)
                 else print "Error Request! Please check your connection and try again"

       
repeatMessage :: Int -> String -> IO ()
repeatMessage 0 _ = print "not repeats count"
repeatMessage r t = do
        requestNew <- parseRequest $ t
        responseForMessage <- httpLbs requestNew
        repeatMessage (r - 1) t

urlHelpMessage :: TelegramBot -> Maybe TelegramMessage -> String
urlHelpMessage bot mess = ap ++ i ++ t where
        i  = "?chat_id=" ++ (show $ chat_id $ chat $ fromJust mess)
        t  = "&text=" ++ ( help $ config bot)
        ap = sendMessage bot 

urlLastMessage :: TelegramBot -> Maybe TelegramMessage -> String
urlLastMessage bot mess = ap ++ i ++ t where
        i  = "?chat_id=" ++ (show $ chat_id $ chat $ fromJust mess)
        t  = "&text=" ++ (text $ fromJust mess)
        ap = sendMessage bot 


findLastMessage :: Integer -> [Update] -> Maybe TelegramMessage
findLastMessage oldId u = if lastId > oldId then Just lastU else Nothing where
        lastU   =  message $ last u
        lastId  =  message_id lastU

logSendFile :: Logging -> FilePath -> IO()
logSendFile l f 
                | l == Debug   = appendFile f "Debug"
                | l == Warning = appendFile f "Warning"
                | l == ErrorS   = appendFile f "Error"




-- keyboardMessageGet 
-- lookup (messText m) keyboardAnswers

-- keyboard :: String
-- keyboard =
--   "{\"keyboard\":[[\"1\",\"2\",\"3\",\"4\",\"5\"]],\"resize_keyboard\": true, \"one_time_keyboard\": true}"










  -- https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/sendMessage?chat_id=4.34218877e8&text=Hello