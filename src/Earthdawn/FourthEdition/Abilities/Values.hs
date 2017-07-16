{-|
Module      : Earthdawn.FourthEdition.Abilities.Values
Description : Earthdawn 4th Edition Ability Values
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Hard-coded values for ability resourcs.

If this resource becomes more dynamic we'll figure out a datastore, but if it
turns out we stop using it then it is what it is.
-}
module Earthdawn.FourthEdition.Abilities.Values where

import qualified Data.Map.Strict as Map (fromList, Map)

import Earthdawn.FourthEdition.Abilities.Types

abilitiesMap :: Map.Map String Ability
abilitiesMap = Map.fromList [ (name a, a) | a <- [heatSight, strongBack, lowLightVision, versatility, increasedWoundThreshold, naturalArmor, gahad, tailCombat, astralSight, flight, increasedPhysicalDefense] ]

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
