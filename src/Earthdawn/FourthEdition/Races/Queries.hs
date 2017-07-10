{-# LANGUAGE OverloadedStrings #-}

module Earthdawn.FourthEdition.Races.Queries
  ( fromName
  , playerRaces
  ) where

import Data.Text (Text, concat)
import Numeric.Natural (Natural)

import qualified Data.Map as Map (elems, fromList, Map, lookup)

import Earthdawn.FourthEdition.Races.Types

playerRaces :: [Race]
playerRaces = Map.elems races

fromName :: Text -> Maybe Race
fromName = flip Map.lookup races

races :: Map.Map Text Race
races = Map.fromList [ (name r, r) | r <- [elf, dwarf, human, obsidiman, ork, troll, tSkrang, windling] ]

elf :: Race
elf = Race
  { collectionURL   = ""
  , name            = "elf"
  , dexterity       = 12
  , strength        = 10
  , toughness       = 8
  , perception      = 11
  , willpower       = 11
  , charisma        = 11
  , movementRate    = MovementRate 14 0
  , karmaModifier   = 4
  }

dwarf :: Race
dwarf = Race
  { collectionURL   = ""
  , name            = "dwarf"
  , dexterity       = 9
  , strength        = 10
  , toughness       = 12
  , perception      = 11
  , willpower       = 11
  , charisma        = 10
  , movementRate    = MovementRate 10 0
  , karmaModifier   = 4
  }

human :: Race
human = Race
  { collectionURL   = ""
  , name            = "human"
  , dexterity       = 10
  , strength        = 10
  , toughness       = 10
  , perception      = 10
  , willpower       = 10
  , charisma        = 10
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 5
  }

obsidiman :: Race
obsidiman = Race
  { collectionURL   = ""
  , name            = "obsidiman"
  , dexterity       = 8
  , strength        = 18
  , toughness       = 13
  , perception      = 9
  , willpower       = 10
  , charisma        = 9
  , movementRate    = MovementRate 10 0
  , karmaModifier   = 3
  }

ork :: Race
ork = Race
  { collectionURL   = ""
  , name            = "ork"
  , dexterity       = 10
  , strength        = 13
  , toughness       = 11
  , perception      = 10
  , willpower       = 8
  , charisma        = 9
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 5
  }

troll :: Race
troll = Race
  { collectionURL   = ""
  , name            = "troll"
  , dexterity       = 10
  , strength        = 14
  , toughness       = 12
  , perception      = 9
  , willpower       = 11
  , charisma        = 10
  , movementRate    = MovementRate 14 0
  , karmaModifier   = 3
  }

tSkrang :: Race
tSkrang = Race
  { collectionURL   = ""
  , name            = "tskrang" -- URL friendly
  , dexterity       = 11
  , strength        = 10
  , toughness       = 11
  , perception      = 10
  , willpower       = 10
  , charisma        = 11
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 4
  }

windling :: Race
windling = Race
  { collectionURL   = ""
  , name            = "windling"
  , dexterity       = 11
  , strength        = 4
  , toughness       = 8
  , perception      = 11
  , willpower       = 10
  , charisma        = 12
  , movementRate    = MovementRate 6 16
  , karmaModifier   = 6
  }
