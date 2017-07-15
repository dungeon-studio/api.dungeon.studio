{-|
Module      : Earthdawn.FourthEdition.Races.Values
Description : Earthdawn 4th Edition Race Values
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Hard-coded values for race resources.

If this requires more dynamic content we'll figure out how to replace this with
a datastore.
-}
module Earthdawn.FourthEdition.Races.Values where

import Earthdawn.FourthEdition.Abilities.Values
import Earthdawn.FourthEdition.Races.Types

elf :: Race
elf = Race
  { name            = "elf"
  , dexterity       = 12
  , strength        = 10
  , toughness       = 8
  , perception      = 11
  , willpower       = 11
  , charisma        = 11
  , movementRate    = MovementRate 14 0
  , karmaModifier   = 4
  , abilities       = [lowLightVision]
  }

dwarf :: Race
dwarf = Race
  { name            = "dwarf"
  , dexterity       = 9
  , strength        = 10
  , toughness       = 12
  , perception      = 11
  , willpower       = 11
  , charisma        = 10
  , movementRate    = MovementRate 10 0
  , karmaModifier   = 4
  , abilities       = [heatSight, strongBack]
  }

human :: Race
human = Race
  { name            = "human"
  , dexterity       = 10
  , strength        = 10
  , toughness       = 10
  , perception      = 10
  , willpower       = 10
  , charisma        = 10
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 5
  , abilities       = [versatility]
  }

obsidiman :: Race
obsidiman = Race
  { name            = "obsidiman"
  , dexterity       = 8
  , strength        = 18
  , toughness       = 13
  , perception      = 9
  , willpower       = 10
  , charisma        = 9
  , movementRate    = MovementRate 10 0
  , karmaModifier   = 3
  , abilities       = [increasedWoundThreshold, naturalArmor]
  }

ork :: Race
ork = Race
  { name            = "ork"
  , dexterity       = 10
  , strength        = 13
  , toughness       = 11
  , perception      = 10
  , willpower       = 8
  , charisma        = 9
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 5
  , abilities       = [gahad, lowLightVision]
  }

troll :: Race
troll = Race
  { name            = "troll"
  , dexterity       = 10
  , strength        = 14
  , toughness       = 12
  , perception      = 9
  , willpower       = 11
  , charisma        = 10
  , movementRate    = MovementRate 14 0
  , karmaModifier   = 3
  , abilities       = [heatSight]
  }

tSkrang :: Race
tSkrang = Race
  { name            = "tskrang" -- URL friendly
  , dexterity       = 11
  , strength        = 10
  , toughness       = 11
  , perception      = 10
  , willpower       = 10
  , charisma        = 11
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 4
  , abilities       = [tailCombat]
  }

windling :: Race
windling = Race
  { name            = "windling"
  , dexterity       = 11
  , strength        = 4
  , toughness       = 8
  , perception      = 11
  , willpower       = 10
  , charisma        = 12
  , movementRate    = MovementRate 6 16
  , karmaModifier   = 6
  , abilities       = [astralSight, flight, increasedPhysicalDefense]
  }