{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Earthdawn.FourthEdition.API
  ( API
  , server
  ) where

import Data.Text (Text)
import Servant ((:>), Server)

import qualified Data.Text as T (append)

import qualified Earthdawn.FourthEdition.Races.API as Races

type API = "races" :> Races.API

server :: Text -> Server API
server b = Races.server (T.append b "/races")
