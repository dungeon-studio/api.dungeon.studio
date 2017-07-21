{-|
Module      : Earthdawn.FourthEdition.Races.Queries
Description : Earthdawn 4th Edition Race Queries
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for persisted race resources.
-}
module Earthdawn.FourthEdition.Races.Queries
  ( races
  , fromName
  ) where

import qualified Data.Map as Map (elems, lookup)

import Earthdawn.FourthEdition.Races.Types
import Earthdawn.FourthEdition.Races.Values

-- | Retrieve all 'Race's.
races :: [Race]
races = Map.elems racesMap

-- | Retrieve a 'Race' by name.
fromName :: String -> Maybe Race
fromName = flip Map.lookup racesMap
