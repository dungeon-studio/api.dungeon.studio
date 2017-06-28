module Earthdawn.Attributes where

import Numeric.Natural (Natural)

type Dexterity = Natural
type Strength = Natural
type Toughness = Natural
type Perception = Natural
type Willpower = Natural
type Charisma = Natural

data Attributes = Attributes { dexterity :: Dexterity
                             , strength :: Strength
                             , toughness :: Toughness
                             , perception :: Perception
                             , willpower :: Willpower
                             , charisma :: Charisma
                             }
