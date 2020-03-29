{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Request.Request where

import Control.Concurrent.Async
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import Network.HTTP.Conduit


import Bot.EchoBot
