module Earthdawn.Races where

import Numeric.Natural (Natural)

import Earthdawn.Attributes
import Earthdawn.RacialAbilities

data Race = Windling

-- TODO Move to configuration files?
startingAttributes :: Race -> Attributes
startingAttributes Windling  = Attributes { dexterity  = 11
                                          , strength   =  4
                                          , toughness  =  8
                                          , perception = 11
                                          , willpower  = 10
                                          , charisma   = 12
                                          }

movementRate :: Race -> Natural
movementRate Windling  =  6

flyingRate :: Race -> Natural
flyingRate Windling = 16
flyingRate _        = 0

karmaModifier :: Race -> Natural
karmaModifier Windling  = 6

racialAbilities :: Race -> [RacialAbility]
racialAbilities Windling  = [ AstralSight, Flight, IncreasedPhysicalDefense ]
