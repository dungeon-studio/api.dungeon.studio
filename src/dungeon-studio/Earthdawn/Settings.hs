{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Earthdawn.Settings
Description : Settings for Earthdawn Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Settings for Earthdawn resources.
-}
module Earthdawn.Settings
  ( Settings
      ( Settings
      , neo4j
      )
  , settings
  ) where

import Database.Bolt (Pipe)
import Data.Pool (Pool)
import Network.URI (URI)

import qualified Environment as E

-- | Earthdawn Settings.
data Settings = Settings
  { neo4j       :: Pool Pipe
  , disciplines :: URI
  , races       :: URI
  }

-- | Construct 'Settings' from 'Environment'.
settings :: E.Environment -> Settings
settings E.Environment{E.neo4j = n, E.disciplines = d, E.races = r} = Settings
                                                                        { neo4j = n
                                                                        , disciplines = d
                                                                        , races = r
                                                                        }
