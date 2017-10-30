{-|
Module      : Settings
Description : Settings for api.dungeon.studio
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Settings for api.dungeon.studio.
-}
module Settings
  ( Settings
      ( Settings
      , earthdawn
      )
  , settings
  ) where

import Environment

import qualified Earthdawn.Settings as Earthdawn

-- | 'Settings' for api.dungeon.studio.
newtype Settings = Settings
  { earthdawn :: Earthdawn.Settings
  }

-- | 'Settings' from 'Environment'.
settings :: Environment -> Settings
settings = Settings . Earthdawn.settings
