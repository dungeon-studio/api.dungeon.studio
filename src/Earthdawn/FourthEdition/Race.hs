{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Earthdawn.FourthEdition.Race where

import Data.Aeson
import Data.Text (Text, pack, concat, takeEnd, dropWhile, reverse)
import Numeric.Natural (Natural)

import Collection.Type

data MovementRate = MovementRate Natural Natural

instance Show MovementRate where
  show (MovementRate w 0) = show w
  show (MovementRate w f) = show w ++ "/" ++ show f

instance ToJSON MovementRate where
  toJSON = toJSON . show

data RacialAbility = AstralSight
                   | Flight
                   | Gahad
                   | HeatSight
                   | IncreasedPhysicalDefense
                   | IncreasedWoundThreshold
                   | LowLightVision
                   | NaturalArmor
                   | StrongBack
                   | TailCombat
                   | Versatility


instance Show RacialAbility where
  show AstralSight              = "astral-sight"
  show Flight                   = "flight"
  show Gahad                    = "gahad"
  show HeatSight                = "heat-sight"
  show IncreasedPhysicalDefense = "increased-physical-defense"
  show IncreasedWoundThreshold  = "increased-wound-threshold"
  show LowLightVision           = "low-light-vision"
  show NaturalArmor             = "natural-armor"
  show StrongBack               = "strong-back"
  show TailCombat               = "tail-combat"
  show Versatility              = "versatility"

instance ToJSON RacialAbility where
  toJSON = toJSON . show -- TODO use titleized names?

data Race = Race
  { collectionURL   :: Text
  , name            :: Text
  , dexterity       :: Natural
  , strength        :: Natural
  , toughness       :: Natural
  , perception      :: Natural
  , willpower       :: Natural
  , charisma        :: Natural
  , movementRate    :: MovementRate
  , karmaModifier   :: Natural
  , racialAbilities :: [RacialAbility]
  }

url :: Race -> Text
url r = Data.Text.concat [collectionURL r, "/", name r]

instance ToJSON Race where
  toJSON Race{..} = object
    [ "dexterity"        .= dexterity
    , "strength"         .= strength
    , "toughness"        .= toughness
    , "perception"       .= perception
    , "willpower"        .= willpower
    , "charisma"         .= charisma
    , "movement_rate"    .= movementRate
    , "karma_modifier"   .= karmaModifier
    , "racial_abilities" .= racialAbilities
    ]

instance ToCollection Race where
  toCollection r = Collection
    { cVersion  = "1.0"
    , cHref     = url r
    , cLinks    = []
    , cItems    = [toItem r]
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    }

instance ToCollection [Race] where
  toCollection rs = Collection
    { cVersion  = "1.0"
    , cHref     = collectionURL
    , cLinks    = []
    , cItems    = map toItem rs
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    } where collectionURL = dropRace $ url (head rs)
            dropRace = Data.Text.reverse . takeEnd 1 . Data.Text.dropWhile (/='/') . Data.Text.reverse

toItem :: Race -> Item
toItem r = Item
  { iHref = url r
  , iData = [ Data { dName = "dexterity",      dValue = Just . pack . show $ dexterity r,     dPrompt = Just "Dexterity"      }
            , Data { dName = "strength",       dValue = Just . pack . show $ strength r,      dPrompt = Just "Strength"       }
            , Data { dName = "toughness",      dValue = Just . pack . show $ toughness r,     dPrompt = Just "Toughness"      }
            , Data { dName = "perception",     dValue = Just . pack . show $ perception r,    dPrompt = Just "Perception"     }
            , Data { dName = "willpower",      dValue = Just . pack . show $ willpower r,     dPrompt = Just "Willpower"      }
            , Data { dName = "charisma",       dValue = Just . pack . show $ charisma r,      dPrompt = Just "Charisma"       }
            , Data { dName = "movement_rate",  dValue = Just . pack . show $ movementRate r,  dPrompt = Just "Movement Rate"  }
            , Data { dName = "karma_modifier", dValue = Just . pack . show $ karmaModifier r, dPrompt = Just "Karma Modifier" }
            ]
  , iLinks = map toLink $ racialAbilities r
  }
  where toLink :: RacialAbility -> Link
        toLink a = Link
          { lHref = Data.Text.concat [url r, pack "/abilities/", pack $ show a]
          , lRel = "ability"
          , lName = Nothing
          , lRender = Nothing
          , lPrompt = Nothing
          }

elf :: Text -> Race
elf u = Race
  { collectionURL   = u
  , name            = "elf"
  , dexterity       = 12
  , strength        = 10
  , toughness       = 8
  , perception      = 11
  , willpower       = 11
  , charisma        = 11
  , movementRate    = MovementRate 14 0
  , karmaModifier   = 4
  , racialAbilities = [LowLightVision]
  }

human :: Text -> Race
human u = Race
  { collectionURL   = u
  , name            = "human"
  , dexterity       = 10
  , strength        = 10
  , toughness       = 10
  , perception      = 10
  , willpower       = 10
  , charisma        = 10
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 5
  , racialAbilities = [Versatility]
  }

obsidiman :: Text -> Race
obsidiman u = Race
  { collectionURL   = u
  , name            = "obsidiman"
  , dexterity       = 8
  , strength        = 18
  , toughness       = 13
  , perception      = 9
  , willpower       = 10
  , charisma        = 9
  , movementRate    = MovementRate 10 0
  , karmaModifier   = 3
  , racialAbilities = [IncreasedWoundThreshold, NaturalArmor]
  }

ork :: Text -> Race
ork u = Race
  { collectionURL   = u
  , name            = "ork"
  , dexterity       = 10
  , strength        = 13
  , toughness       = 11
  , perception      = 10
  , willpower       = 8
  , charisma        = 9
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 5
  , racialAbilities = [Gahad, LowLightVision]
  }

troll :: Text -> Race
troll u = Race
  { collectionURL   = u
  , name            = "troll"
  , dexterity       = 10
  , strength        = 14
  , toughness       = 12
  , perception      = 9
  , willpower       = 11
  , charisma        = 10
  , movementRate    = MovementRate 14 0
  , karmaModifier   = 3
  , racialAbilities = [HeatSight]
  }

tSkrang :: Text -> Race
tSkrang u = Race
  { collectionURL   = u
  , name            = "tskrang" -- URL friendly
  , dexterity       = 11
  , strength        = 10
  , toughness       = 11
  , perception      = 10
  , willpower       = 10
  , charisma        = 11
  , movementRate    = MovementRate 12 0
  , karmaModifier   = 4
  , racialAbilities = [TailCombat]
  }

windling :: Text -> Race
windling u = Race
  { collectionURL   = u
  , name            = "windling"
  , dexterity       = 11
  , strength        = 4
  , toughness       = 8
  , perception      = 11
  , willpower       = 10
  , charisma        = 12
  , movementRate    = MovementRate 6 16
  , karmaModifier   = 6
  , racialAbilities = [AstralSight, Flight, IncreasedPhysicalDefense]
  }
