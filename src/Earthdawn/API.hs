{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Earthdawn.API
Description : HTTP API for Earthdawn Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for Earthdawn resources.
-}
module Earthdawn.API
  ( API
  , server
  ) where

import Data.Maybe (fromJust)
import Network.URI (URI, parseURIReference, relativeTo)
import Servant ((:>), Server)

import qualified Earthdawn.FourthEdition.API as FourthEdition

-- | An API type for Earthdawn.
type API = "4e" :> FourthEdition.API

-- | Constructs an Earthdawn 'Servant' 'Server' given a URL path prefix.
server :: URI -> Server API
server b = FourthEdition.server $ fromJust (parseURIReference "4e") `relativeTo` b
