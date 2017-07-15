{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Servant

import qualified Earthdawn.FourthEdition.Abilities.API as Abilities
import qualified Earthdawn.FourthEdition.Races.API as Races

-- | An API type for Earthdawn 4th Edition.
type API = "abilities" :> Abilities.API
      :<|> "races" :> Races.API

-- | Constructs an Earthdawn 4th Edition 'Servant' 'Server' given a URL path prefix.
server :: String -> Server API
server b = Abilities.server (b ++ "/abilities")
      :<|> Races.server (b ++ "/races")
