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

import Servant

import Earthdawn.Settings

import qualified Earthdawn.FourthEdition.Abilities.API as Abilities
import qualified Earthdawn.FourthEdition.Characters.API as Characters
import qualified Earthdawn.FourthEdition.Disciplines.API as Disciplines
import qualified Earthdawn.FourthEdition.Races.API as Races

-- | "Servant" API for Earthdawn 4th Edition.
--
--   Implemented Resources:
--
--   * "Earthdawn.FourthEdition.Abilities.API"
--   * "Earthdawn.FourthEdition.Characters.API"
--   * "Earthdawn.FourthEdition.Disciplines.API"
--   * "Earthdawn.FourthEdition.Races.API"
type API = "abilities" :> Abilities.API
      :<|> "characters" :> Characters.API
      :<|> "disciplines" :> Disciplines.API
      :<|> "races" :> Races.API

-- | "Servant" "Server" for Earthdawn 4th Edition.
server :: String -> Settings -> Server API
server b s = Abilities.server (b ++ "/abilities")
      :<|> Characters.server (b ++ "/characters") s
      :<|> Disciplines.server (b ++ "/disciplines")
      :<|> Races.server (b ++ "/races")
