{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : API
Description : HTTP API for api.dungeon.studio
Copyright   : (c) Alex Brandt, 2017
License     : MIT

API for api.dungeon.studio.
-}
module API
  ( API
  , server
  ) where

import Network.URI (URI)
import Servant ((:>), Server)

import Settings (Settings)

import qualified Characters.API as C

-- | "Servant" API for api.dungeon.studio.
type API = "characters" :> C.API

-- | "Servant" 'Server' for api.dungeon.studio.
server :: URI -> Settings -> Server API
server = C.server
