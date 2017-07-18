{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Earthdawn.FourthEdition.Races.Types
Description : Earthdawn 4th Edition Race Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Race and Collection Types.
-}
module Earthdawn.FourthEdition.Races.Types
  ( RaceCollection
      ( RaceCollection
      )
  , Race
      ( Race
      , name
      , dexterity
      , strength
      , toughness
      , perception
      , willpower
      , charisma
      , movementRate
      , karmaModifier
      , abilities
      )
  , MovementRate(MovementRate)
  ) where

import Network.URI (URI)
import Numeric.Natural (Natural)

import qualified Data.Text as T (pack)

import Data.CollectionJSON
import Earthdawn.FourthEdition.Abilities.Types (Ability)
import Internal.URI

-- | @application/vnd.collection+json for 'Race'@
data RaceCollection = RaceCollection URI [Race]

instance ToCollection RaceCollection where
  toCollection (RaceCollection u rs) = Collection
    { cVersion  = "1.0"
    , cHref     = u
    , cLinks    = []
    , cItems    = map (toItem u) rs
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    }

-- | Earthdawn race representation type.
data Race = Race
  { name          :: String
  , dexterity     :: Natural
  , strength      :: Natural
  , toughness     :: Natural
  , perception    :: Natural
  , willpower     :: Natural
  , charisma      :: Natural
  , movementRate  :: MovementRate
  , karmaModifier :: Natural
  , abilities     :: [Ability]
  }

toItem :: URI -> Race -> Item
toItem u r = Item
  { iHref = u'
  , iData = [ Datum { dName = "dexterity",      dValue = Just . T.pack . show $ dexterity r,     dPrompt = Just "DEX"            }
            , Datum { dName = "strength",       dValue = Just . T.pack . show $ strength r,      dPrompt = Just "STR"            }
            , Datum { dName = "toughness",      dValue = Just . T.pack . show $ toughness r,     dPrompt = Just "TOU"            }
            , Datum { dName = "perception",     dValue = Just . T.pack . show $ perception r,    dPrompt = Just "PER"            }
            , Datum { dName = "willpower",      dValue = Just . T.pack . show $ willpower r,     dPrompt = Just "WIL"            }
            , Datum { dName = "charisma",       dValue = Just . T.pack . show $ charisma r,      dPrompt = Just "CHA"            }
            , Datum { dName = "movement_rate",  dValue = Just . T.pack . show $ movementRate r,  dPrompt = Just "Movement Rate"  }
            , Datum { dName = "karma_modifier", dValue = Just . T.pack . show $ karmaModifier r, dPrompt = Just "Karma Modifier" }
            ]
  , iLinks = [ Link { lHref = append u' "abilities", lRel = "abilities", lName = Nothing, lRender = Nothing, lPrompt = Nothing } ]
  }
  where u' = append u $ name r

-- | Convenience type for movement rate.
data MovementRate = MovementRate Natural Natural

instance Show MovementRate where
  show (MovementRate w 0) = show w
  show (MovementRate w f) = show w ++ "/" ++ show f
