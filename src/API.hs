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

import Internal.Network.URI (addPathPart)
import Settings (Settings)

import qualified Characters.API as C

-- | "Servant" API for api.dungeon.studio.
type API = "characters" :> C.API

-- | "Servant" 'Server' for api.dungeon.studio.
server :: URI -> Settings -> Server API
server u = C.server $ u `addPathPart` "characters"
