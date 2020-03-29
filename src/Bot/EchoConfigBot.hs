{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.EchoConfigBot where

import Control.Monad.State
import Data.Maybe (isJust)

class Bot b where
    type BotConfig b = c| c -> b
    type BotMessage b = m| m -> b
    getBotWithConfig :: BotConfig b -> b
    getLastMessage :: StateT b IO (Maybe b)
    sendMessageTo :: BotMessage b -> String -> StateT b IO ()




