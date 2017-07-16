{-|
Module      : Earthdawn.FourthEdition.Races.Queries
Description : Earthdawn 4th Edition Race Persistence Accessors
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for race resources that are persisted by internal means.
-}
module Earthdawn.FourthEdition.Races.Queries
  ( races
  , fromName
  ) where

import qualified Data.Map as Map (elems, lookup)

import Earthdawn.FourthEdition.Races.Types
import Earthdawn.FourthEdition.Races.Values

races :: [Race]
races = Map.elems racesMap

fromName :: String -> Maybe Race
fromName = flip Map.lookup racesMap
