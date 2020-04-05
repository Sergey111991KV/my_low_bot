{-# LANGUAGE OverloadedStrings #-}


module Telegram.KeyboardTelegram where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Telegram.TelegramConfig
import Telegram.RequestTelegram 
import qualified Network.HTTP.Types as HTTP
import Data.ByteString.Char8 (pack)


keyboardMessageSend :: Integer -> String -> IO ()
keyboardMessageSend chatId sendUrl = do
        sendTelegram sendUrl [ ("chat_id", show chatId), ("text", "please select repeats count:"), ("reply_markup", keyboard)]


-- sendTelegram :: String -> [RequestParam] -> IO ()
-- sendTelegram url params = do
--         let body = buildRequestBody params
--         manager <- newManager tlsManagerSettings
--         request <- buildRequest url  body
--         print (buildQueryParams params)
--         response <- httpLbs request manager
--         return ()
    
-- buildRequest :: String -> RequestBody -> IO Request
-- buildRequest url body = do
--           nakedRequest <- parseRequest url
--           print "bbbb"
--           return
--             (nakedRequest
--                { method = "POST"
--                , requestBody = body
--                , requestHeaders = [(HTTP.hContentType, "application/json")]
--                })
        

-- buildRequestBody :: [RequestParam] -> RequestBody
-- buildRequestBody =
--                  RequestBodyBS . pack . ('{' :) . foldr buildBody "}" . fmap getParStr
--                  where
--                    getParStr (parName, parVal) =
--                      strToParam parName ++ ":" ++ strToParam parVal ++ ","
--                    strToParam s@('{':xs) = s
--                    strToParam s = "\"" ++ s ++ "\""
--                    buildBody p "}" = init p ++ "}"
--                    buildBody p r = p ++ r
               
-- buildQueryParams :: [RequestParam] -> String
-- buildQueryParams = reverse . foldl addParam ""
--                      where
--                        addParam [] par = getParStr par
--                        addParam pars par = pars ++ ('&' : getParStr par)
--                        getParStr (parName, parVal) = parName ++ "=" ++ parVal





sendTelegram :: String -> [RequestParam] -> IO ()
sendTelegram url params = do
            let body = buildRequestBody params
            print body
            manager <- newManager tlsManagerSettings
            request <- buildRequest url body
            response <- httpLbs request manager
            return ()
                       
buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
        nakedRequest <- parseRequest url
        return
                (nakedRequest
                    { method = "POST"
                    , requestBody = body
                    , requestHeaders = [(HTTP.hContentType, "application/json")]
                    })
 
                              

buildRequestBody :: [RequestParam] -> RequestBody
buildRequestBody =
  RequestBodyBS . pack . ('{' :) . foldr buildBody "}" . fmap getParStr
  where
    getParStr (parName, parVal) =
      strToParam parName ++ ":" ++ strToParam parVal ++ ","
    strToParam s@('{':xs) = s
    strToParam s = "\"" ++ s ++ "\""
    buildBody p "}" = init p ++ "}"
    buildBody p r = p ++ r

buildQueryParams :: [RequestParam] -> String
buildQueryParams = reverse . foldl addParam ""
  where
    addParam [] par = getParStr par
    addParam pars par = pars ++ ('&' : getParStr par)
    getParStr (parName, parVal) = parName ++ "=" ++ parVal



sendText :: String -> Integer -> String -> IO ()
sendText  txt chatId sendUrl
  | txt == keyboard =
      (sendTelegram
         sendUrl
         [ ("chat_id", show chatId)
         , ("text", "please select repeats count:")
         , ("reply_markup", keyboard)
         ]) 
   
  | otherwise =
    sendTelegram sendUrl [("chat_id", show chatId), ("text", txt)]
