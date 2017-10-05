{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Earthdawn.FourthEdition.API
Description : HTTP API for Earthdawn 4th Edition Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for Earthdawn 4th Edition resources.
-}
module Earthdawn.FourthEdition.API
  ( API
  , server
  ) where

import Servant ((:>), Server)

import Earthdawn.Settings

import qualified Earthdawn.FourthEdition.Characters.API as Characters

-- | "Servant" API for Earthdawn 4th Edition.
--
--   Implemented Resources:
--
--   * "Earthdawn.FourthEdition.Characters.API"
type API = "characters" :> Characters.API

-- | "Servant" "Server" for Earthdawn 4th Edition.
server :: String -> Settings -> Server API
server b = Characters.server (b ++ "/characters")
