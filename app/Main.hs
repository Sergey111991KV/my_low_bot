{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Control.Concurrent.Async
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple
-- import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import Network.HTTP.Conduit

import Telegram.RequestTelegram
import Telegram.Telegram

jsonURL :: String
jsonURL = "https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/getUpdates"

-- getJSON :: IO B.ByteString
-- getJSON = simpleHttp jsonURL

main :: IO ()
main = do
    telegramConfig <- getTelegramConfig
    runTelegramEcho telegramConfig

   
    -- print $  response
    -- request <- parseRequest "http://httpbin.org/get"
    -- d  <- (eitherDecode <$> getJSON) :: IO (Either String Updates)
    -- case d of
    --   Left err -> putStrLn err
    --   Right ps  -> print ( message (last (result ps)))


