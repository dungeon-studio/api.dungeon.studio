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

import Servant ((:>), Server)

import qualified Earthdawn.FourthEdition.API as FourthEdition

-- | An API type for Earthdawn.
type API = "4e" :> FourthEdition.API

-- | Constructs an Earthdawn 'Servant' 'Server' given a URL path prefix.
server :: String -> Server API
server b = FourthEdition.server $ b ++ "/4e"
