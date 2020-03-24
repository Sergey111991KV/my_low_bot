{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import Control.Concurrent.Async
import           Data.Aeson.Parser           
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status   (statusCode)
import Data.Aeson
import Entities
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

myparse :: Updates -> [Update]
myparse (Updates a s) = s

telmes :: Update -> TelegramMessage
telmes (Update a c) = c

myparse' :: Updates -> TelegramMessage
myparse' x = telmes $ last $ (myparse x)

jsonURL :: String
jsonURL = "https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/getUpdates"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

main = do
    d  <- (eitherDecode <$> getJSON) :: IO (Either String Updates)
    case d of
      Left err -> putStrLn err
      Right ps  -> print (myparse' ps)

