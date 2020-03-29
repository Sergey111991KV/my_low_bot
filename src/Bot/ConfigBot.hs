-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleContexts #-}



module Bot.ConfigBot where

import Control.Monad.State
import Data.Maybe (isJust)

inm = 123 :: Integer

-- class Message (BotMessage b) => Bot b where
--     type BotConfig b = c| c -> b
--     type BotMessage b = m| m -> b
--     getBotWithConfig :: BotConfig b -> b
--     getLastMessage :: StateT b IO (Maybe (BotMessage b))
--     sendMessageTo :: Id (BotMessage b) -> String -> StateT b IO ()

-- class Bot m => EchoBot m where
--     helpMessage :: b -> String
--     helpMessage _ = "Default echo-bot help message"
--     repeatsCount :: b -> Int
--     repeatsTxt :: b -> String
--     isWaitingForRepeats :: b -> Bool
--     setWaitingForRepeats :: Bool -> b -> b
--     setRepeatsCount :: Int -> b -> b
--     tryGetRepeatsCount :: b -> Maybe Int


-- startFirstEchoBot :: EchoBot b => BotConfig b -> IO ()
-- startFirstEchoBot = evalStateT startSecondEchoBot . getBotWithConfig

-- startSecondEchoBot :: EchoBot b => StateT b IO ()
-- startSecondEchoBot = do
--   m <- getLastMessage
--   maybe (return ()) echoMessage m
--   startSecondEchoBot

