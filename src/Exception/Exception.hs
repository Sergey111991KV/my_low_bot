{-# LANGUAGE OverloadedStrings #-}

module Exception.Exception where

import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Lazy as B
import Logging.Logging


handleGetException :: LogConfig -> SomeException -> IO B.ByteString
handleGetException LogConfig { logFile = logF
                             , logLevel = logL
                             , logConsole = logCons
                             } e = do
  let errMess = show e
      logMess =
        "\nWarning: server did not send information about new messages: they may be lost: " ++
        show e
  when
    (logL <= Warning)
    (do when logCons (print logMess)
        appendFile logF logMess)
  throwIfInterrupt e errMess
  return ""

-- handlePollException :: LogConfig -> SomeException -> IO B.ByteString
-- handlePollException lc e = do
--   let errMess = show e
--       logMess =
--         "\nWarning: server did not send information about new messages: they may be lost: " ++
--         show e
--   logFileConsole lc e logMess errMess
--   return ""

-- handleSendException :: LogConfig -> SomeException -> IO ()
-- handleSendException lc e = do
--   let errMess = show e
--       logMess =
--         "\nWarning: server did not send information about new messages: they may be lost: " ++
--         errMess
--   logFileConsole lc e logMess errMess

-- logFileConsole :: LogConfig -> SomeException -> String -> String -> IO ()
-- logFileConsole lc e logMess errMess = do
--   when (logConsole lc) (print logMess)
--     appendFile (logFile lc) logMess
--   throwIfInterrupt e errMess

throwIfInterrupt :: SomeException -> String -> IO ()
throwIfInterrupt e "user interrupt" = print (show e) >> throw e
throwIfInterrupt _ _ = return ()

-- handleGetException :: LogConfig -> SomeException -> IO B.ByteString
-- handleGetException LogConfig { logFile = logF
--                              , logLevel = logL
--                              , logConsole = logCons
--                              } e = do













------ НЕ понимаю как обработать exeption при получении ответа
-- eresponse <- try $ httpLBS "http://does-not-exist"

--     case eresponse of
--         Left e -> print (e :: HttpException)
--         Right response -> L8.putStrLn $ getResponseBody response -- это при обычном вызове, а как при json

