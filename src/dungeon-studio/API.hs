{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:     : API
Description : HTTP API for api.dungeon.studio
Copyright   : (c) Alex Brandt, 2017
License     : MIT

API for api.dungeon.studio.
-}
module API
  ( API
  , server
  ) where

import Servant ((:>), Server)

import Settings

import qualified Earthdawn.API as Earthdawn

-- | "Servant" API for our various games.
--
--   Implemented Games:
--
--   * "Earthdawn.API"
type API = "earthdawn" :> Earthdawn.API

-- | "Servant" 'Server' for api.dungeon.studio.
server :: Settings -> Server API
server = Earthdawn.server "/earthdawn" . earthdawn
