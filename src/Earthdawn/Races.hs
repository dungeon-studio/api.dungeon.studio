module Earthdawn.Races where

import Numeric.Natural (Natural)

import Earthdawn.Attributes
import Earthdawn.RacialAbilities

data Race = Elf
          | Human
          | Obsidiman
          | Ork
          | Troll
          | T'Skrang
          | Windling

startingAttributes :: Race -> Attributes
startingAttributes Elf       = Attributes { dexterity  = 12
                                          , strength   = 10
                                          , toughness  =  8
                                          , perception = 11
                                          , willpower  = 11
                                          , charisma   = 11
                                          }
startingAttributes Human     = Attributes { dexterity  = 10
                                          , strength   = 10
                                          , toughness  = 10
                                          , perception = 10
                                          , willpower  = 10
                                          , charisma   = 10
                                          }
startingAttributes Obsidiman = Attributes { dexterity  =  8
                                          , strength   = 18
                                          , toughness  = 13
                                          , perception =  9
                                          , willpower  = 10
                                          , charisma   =  9
                                          }
startingAttributes Ork       = Attributes { dexterity  = 10
                                          , strength   = 13
                                          , toughness  = 11
                                          , perception = 10
                                          , willpower  =  8
                                          , charisma   =  9
                                          }
startingAttributes Troll     = Attributes { dexterity  = 10
                                          , strength   = 14
                                          , toughness  = 12
                                          , perception =  9
                                          , willpower  = 11
                                          , charisma   = 10
                                          }
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
movementRate Elf       = 14
movementRate Human     = 12
movementRate Obsidiman = 10
movementRate Ork       = 12
movementRate Troll     = 14
movementRate T'Skrang  = 12
movementRate Windling  =  6

flyingRate :: Race -> Natural
flyingRate Windling = 16
flyingRate _        = 0

karmaModifier :: Race -> Natural
karmaModifier Elf       = 4
karmaModifier Human     = 5
karmaModifier Obsidiman = 3
karmaModifier Ork       = 5
karmaModifier Troll     = 3
karmaModifier T'Skrang  = 4
karmaModifier Windling  = 6

racialAbilities :: Race -> [RacialAbility]
racialAbilities Elf       = [ LowLightVision ]
racialAbilities Human     = [ Versatility ]
racialAbilities Obsidiman = [ IncreasedWoundThreshold, NaturalArmor ]
racialAbilities Ork       = [ Gahad, LowLightVision ]
racialAbilities Troll     = [ HeatSight ]
racialAbilities T'Skrang  = [ TailCombat ]
racialAbilities Windling  = [ AstralSight, Flight, IncreasedPhysicalDefense ]
