{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Editions.Fourth.Race
Description : Earthdawn 4th Edition Race Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Race Types.
-}
module Editions.Fourth.Race
  ( Race
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
  , MovementRate
      ( MovementRate
      )
  ) where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | Earthdawn race.
data Race = Race
  { name          :: Text
  , dexterity     :: Natural
  , strength      :: Natural
  , toughness     :: Natural
  , perception    :: Natural
  , willpower     :: Natural
  , charisma      :: Natural
  , movementRate  :: MovementRate
  , karmaModifier :: Natural
  , abilities     :: [Text]
  }

instance FromJSON Race where
  parseJSON = withObject "Race" $ \ o -> do
    name          <- o .: "name"

    dexterity     <- o .: "dexterity"
    strength      <- o .: "strength"
    toughness     <- o .: "toughness"
    perception    <- o .: "perception"
    willpower     <- o .: "willpower"
    charisma      <- o .: "charisma"

    movementRate  <- o .: "movement_rate"
    karmaModifier <- o .: "karma_modifier"

    abilities     <- o .: "abilities"

    return Race{..}

-- | Earthdawn movement rate.
data MovementRate = MovementRate Natural Natural
  deriving (Generic)

instance FromJSON MovementRate
