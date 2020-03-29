{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Telegram.Telegram where

import Data.Maybe
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple 
import qualified Data.ByteString.Lazy as B
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
-- import Network.HTTP.Conduit
import Data.Aeson
import GHC.Generics
import System.Environment
import Text.Read (readMaybe)

import Telegram.RequestTelegram
import Telegram.TelegramConfig


runTelegramEcho :: TelegramConfig -> Integer -> IO ()
runTelegramEcho c i = moveRequest c i


getTelegramConfig :: IO TelegramConfig
getTelegramConfig = do
        let telegramToken = "1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo"
        telegramRepeats <- maybe 1 read <$> lookupEnv "TG_REPEATS"
        telegramHelp    <- fromMaybe ("Echo bot. Repeats every message n times (default n = " ++ show telegramRepeats ++ "). To change n write /repeat") <$> lookupEnv "TG_HELP"
        return $ TelegramConfig telegramToken telegramHelp telegramRepeats

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
        -- let check = sendLastMessage bot  (findLastMessage (lastMessId bot) (result $ getResponseBody response))
        -- let count =  repeats $ config bot
        if d == 200  then do
                let check = sendLastMessage bot  (findLastMessage (lastMessId bot) (result $ getResponseBody response))
                let count = repeats $ config bot
                -- let idMes = message_id $ message $ last (result $ getResponseBody response)
                repeatMessage count check
                threadDelay 10
                moveRequest c i
                
                
     
        else print "( message (last (result ps)))" 
       
repeatMessage :: Int -> String -> IO ()
repeatMessage 0 _ = print "ss"
repeatMessage r t = do
        requestNew <- parseRequest $ t
        responseForMessage <- httpLbs requestNew
        repeatMessage (r - 1) t

sendLastMessage :: TelegramBot -> Maybe TelegramMessage -> String
sendLastMessage bot mess = ap ++ i ++ t where
        i  = "?chat_id=" ++ (show $ chat_id $ chat $ fromJust mess)
        t  = "&text=" ++ (text $ fromJust mess)
        ap = sendMessage bot 
       

-- https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/sendMessage?chat_id=4.34218877e8&text=Hello

findLastMessage :: Integer -> [Update] -> Maybe TelegramMessage
findLastMessage oldId u = if lastId > oldId then Just lastU else Nothing where
        lastU   =  message $ last u
        lastId  =  message_id lastU

    