{-# LANGUAGE DeriveGeneric #-}

module Bot.EchoBot where


import Bot.Bot
import Bot.EchoConfigBot
import Control.Monad.State
        
class Bot b => EchoBot b where

    helpMessage :: b -> String
    helpMessage _ = "Default echo-bot help message"

    repeatsCount :: b -> Int
    repeatsTxt :: b -> String
    isWaitingForRepeats :: b -> Bool
    setWaitingForRepeats :: Bool -> b -> b
    setRepeatsCount :: Int -> b -> b
    -- tryGetRepeatsCount :: BotMessage b -> Maybe Int

-- startEchoBot :: EchoBot b => BotConfig b -> IO ()
-- startEchoBot = evalStateT runEchoBot . getBotWithConfig
-- startEchoBot =  getBotWithConfig


-- runEchoBot :: EchoBot b => StateT b IO ()
-- runEchoBot = do
--       m <- getLastMessage
--       maybe (return ()) echoMessage m
--       runEchoBot
    --   это я тупо скопировал, но не потому что не понимаю - просто ... блин мне это кажется отличным решением 
    --   да я посмотрел решения других, ок? - скажу вам как в фильме) но монаду в монаде я до сих пор разбираю
    

-- Здесь уже общий интерфейс для всех эхо ботов - а именно здесь мы отвечаем на вопросы что отвечать? сколько раз отвечать? 
--  и нужно ли вообще отвечать?