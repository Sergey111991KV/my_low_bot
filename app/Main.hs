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
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import Network.HTTP.Conduit


import Request.Entities


jsonURL :: String
jsonURL = "https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/getUpdates"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

main :: IO ()
main = do
    request <- parseRequest "https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/getUpdates"
    response  <- httpJSON request :: IO (Response Updates)
    let d = (getResponseStatusCode response)
    case d of
      200 -> print (responseBody response)
      _   -> print "( message (last (result ps)))"

   
    -- print $  response
    -- request <- parseRequest "http://httpbin.org/get"
    -- d  <- (eitherDecode <$> getJSON) :: IO (Either String Updates)
    -- case d of
    --   Left err -> putStrLn err
    --   Right ps  -> print ( message (last (result ps)))


