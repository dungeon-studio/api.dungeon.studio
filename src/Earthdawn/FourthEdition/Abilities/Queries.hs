{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Earthdawn.FourthEdition.Abilities.Queries
Description : Earthdawn 4th Edition Ability Persistence Accessors
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for abililty resources that are persisted by internal means.
-}
module Earthdawn.FourthEdition.Abilities.Queries
  ( playerRaceAbilities
  , fromName
  ) where

import qualified Data.Map as Map (elems, fromList, Map, lookup)

import Earthdawn.FourthEdition.Abilities.Types
import Earthdawn.FourthEdition.Abilities.Values

playerRaceAbilities :: [Ability]
playerRaceAbilities = Map.elems abilities

fromName :: String -> Maybe Ability
fromName = flip Map.lookup abilities

abilities :: Map.Map String Ability
abilities = Map.fromList [ (name a, a) | a <- [heatSight, strongBack, lowLightVision, versatility, increasedWoundThreshold, naturalArmor, gahad, tailCombat, astralSight, flight, increasedPhysicalDefense] ]
