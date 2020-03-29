{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
        
module Bot.Bot where
        
import Control.Monad.State
import Data.Maybe (isJust)

import Bot.ConfigBot
        
class Bot b where
    type BotConfig b = c| c -> b
    type BotMessage b = m| m -> b
   
    -- getLastMessage :: StateT b IO (Maybe (BotMessage b))

--  Краткий обзор)) Bot - это самая первая ступень, самый первый интерфейс - поэтому здесь нужно вынести все что может быть и 
--  в эхо боте и в любом другом боте с любой другой функцией - а это получение конфигураций и комманды (от сообщения)