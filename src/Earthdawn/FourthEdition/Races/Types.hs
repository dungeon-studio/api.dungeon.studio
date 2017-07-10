{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Earthdawn.FourthEdition.Races.Types
  ( charisma
  , collectionURL
  , dexterity
  , karmaModifier
  , movementRate
  , MovementRate(MovementRate)
  , name
  , perception
  , Race(Race)
  , strength
  , toughness
  , willpower
  ) where

import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.Text as T (concat, pack)

import Data.Amundsen.Collection

data MovementRate = MovementRate Natural Natural

instance Show MovementRate where
  show (MovementRate w 0) = show w
  show (MovementRate w f) = show w ++ "/" ++ show f

data Race = Race
  { collectionURL :: Text
  , name          :: Text
  , dexterity     :: Natural
  , strength      :: Natural
  , toughness     :: Natural
  , perception    :: Natural
  , willpower     :: Natural
  , charisma      :: Natural
  , movementRate  :: MovementRate
  , karmaModifier :: Natural
  }

url :: Race -> Text
url r = T.concat [collectionURL r, "/", name r]

instance ToCollection [Race] where
  toCollection rs = Collection
    { cVersion  = "1.0"
    , cHref     = collectionURL (head rs) -- TODO what happens when the collection is empty?
    , cLinks    = []
    , cItems    = map toItem rs
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    } where toItem :: Race -> Item
            toItem r = Item
              { iHref = url r
              , iData = [ Data { dName = "dexterity",      dValue = Just . T.pack . show $ dexterity r,     dPrompt = Just "DEX"            }
                        , Data { dName = "strength",       dValue = Just . T.pack . show $ strength r,      dPrompt = Just "STR"            }
                        , Data { dName = "toughness",      dValue = Just . T.pack . show $ toughness r,     dPrompt = Just "TOU"            }
                        , Data { dName = "perception",     dValue = Just . T.pack . show $ perception r,    dPrompt = Just "PER"            }
                        , Data { dName = "willpower",      dValue = Just . T.pack . show $ willpower r,     dPrompt = Just "WIL"            }
                        , Data { dName = "charisma",       dValue = Just . T.pack . show $ charisma r,      dPrompt = Just "CHA"            }
                        , Data { dName = "movement_rate",  dValue = Just . T.pack . show $ movementRate r,  dPrompt = Just "Movement Rate"  }
                        , Data { dName = "karma_modifier", dValue = Just . T.pack . show $ karmaModifier r, dPrompt = Just "Karma Modifier" }
                        ]
              , iLinks = [ Link { lHref = T.concat [url r, "/abilities"], lRel = "abilities", lName = Nothing, lRender = Nothing, lPrompt = Nothing } ]
              }

instance ToCollection Race where
  toCollection r = toCollection [r]

