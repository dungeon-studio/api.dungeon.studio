{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Earthdawn.API
  ( API
  , server
  ) where

import Data.Text (Text)
import Servant ((:>), Server)

import qualified Data.Text as T (append)

import qualified Earthdawn.FourthEdition.API as FourthEdition

type API = "4e" :> FourthEdition.API

server :: Text -> Server API
server b = FourthEdition.server (T.append b "/4e")
