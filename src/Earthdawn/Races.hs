module Earthdawn.Races where

import Numeric.Natural (Natural)

import Earthdawn.Attributes
import Earthdawn.RacialAbilities

data Race = T'Skrang
          | Windling

-- TODO Move to configuration files?
startingAttributes :: Race -> Attributes
startingAttributes T'Skrang  = Attributes { dexterity  = 11
                                          , strength   = 10
                                          , toughness  = 11
                                          , perception = 10
                                          , willpower  = 10
                                          , charisma   = 11
                                          }
startingAttributes Windling  = Attributes { dexterity  = 11
                                          , strength   =  4
                                          , toughness  =  8
                                          , perception = 11
                                          , willpower  = 10
                                          , charisma   = 12
                                          }

movementRate :: Race -> Natural
movementRate T'Skrang  = 12
movementRate Windling  =  6

flyingRate :: Race -> Natural
flyingRate Windling = 16
flyingRate _        = 0

karmaModifier :: Race -> Natural
karmaModifier T'Skrang  = 4
karmaModifier Windling  = 6

racialAbilities :: Race -> [RacialAbility]
racialAbilities T'Skrang  = [ TailCombat ]
racialAbilities Windling  = [ AstralSight, Flight, IncreasedPhysicalDefense ]
