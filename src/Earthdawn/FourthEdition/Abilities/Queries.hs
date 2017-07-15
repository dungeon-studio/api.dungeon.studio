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

playerRaceAbilities :: [Ability]
playerRaceAbilities = Map.elems abilities

fromName :: String -> Maybe Ability
fromName = flip Map.lookup abilities

abilities :: Map.Map String Ability
abilities = Map.fromList [ (name a, a) | a <- [heatSight, strongBack, lowLightVision, versatility, increasedWoundThreshold, naturalArmor, gahad, tailCombat, astralSight, flight, increasedPhysicalDefense] ]

heatSight :: Ability
heatSight = Ability
  { name = "heat-sight"
  }

strongBack :: Ability
strongBack = Ability
  { name = "strong-back"
  }

lowLightVision :: Ability
lowLightVision = Ability
  { name = "low-light-vision"
  }

versatility :: Ability
versatility = Ability
  { name = "versatility"
  }

increasedWoundThreshold :: Ability
increasedWoundThreshold = Ability
  { name = "increased-wound-threshold"
  }

naturalArmor :: Ability
naturalArmor = Ability
  { name = "natural-armor"
  }

gahad :: Ability
gahad = Ability
  { name = "gahad"
  }

tailCombat :: Ability
tailCombat = Ability
  { name = "tail-combat"
  }

astralSight :: Ability
astralSight = Ability
  { name = "astral-sight"
  }

flight :: Ability
flight = Ability
  { name = "flight"
  }

increasedPhysicalDefense :: Ability
increasedPhysicalDefense = Ability
  { name = "increased-physical-defense"
  }
