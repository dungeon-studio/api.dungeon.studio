{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Earthdawn.FourthEdition.Races.Queries
Description : Earthdawn 4th Edition Race Persistence Accessors
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for race resources that are persisted by internal means.
-}
module Earthdawn.FourthEdition.Races.Queries
  ( playerRaces
  , fromName
  ) where

import qualified Data.Map as Map (elems, fromList, Map, lookup)

import Earthdawn.FourthEdition.Races.Types
import Earthdawn.FourthEdition.Races.Values

-- | A list of all races.
playerRaces :: [Race]
playerRaces = Map.elems races

-- | A particular race with the given name.
fromName :: String -> Maybe Race
fromName = flip Map.lookup races

races :: Map.Map String Race
races = Map.fromList [ (name r, r) | r <- [elf, dwarf, human, obsidiman, ork, troll, tSkrang, windling] ]
