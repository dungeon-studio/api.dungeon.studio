{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Earthdawn.Api
  ( Api
  , defaultSettings
  , fourthEditionSettings
  , server
  , Settings
  )
where

import Data.Text (Text, append)
import Servant
import Servant.API

import qualified Earthdawn.FourthEdition.Api as FourthEdition

type Api = "4e" :> FourthEdition.Api

newtype Settings = Settings
  { fourthEditionSettings :: FourthEdition.Settings
  }

defaultSettings = Settings FourthEdition.defaultSettings

server :: Text -> Settings -> Server Api
server u = FourthEdition.server (Data.Text.append u "/4e") . fourthEditionSettings
