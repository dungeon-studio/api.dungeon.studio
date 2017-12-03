{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Settings
Description : Settings
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Settings.
-}
module Settings
  ( Settings
      ( Settings
      , neo4j
      , disciplines
      , races
      )
  , settings
  ) where

import Control.Monad.IO.Class (MonadIO)
import Database.Bolt (Pipe)
import Data.Pool (Pool)
import Network.URI (URI)

import Environment
import Internal.BoltPool.Environment (toPool)

-- | Earthdawn Settings.
data Settings = Settings
  { neo4j       :: Pool Pipe
  , disciplines :: URI
  , races       :: URI
  }

-- | Construct 'Settings' from 'Environment'.
settings :: (MonadIO m) => Environment -> m Settings
settings Environment{..} = 
  do n <- toPool neo4jEnvironment
     return $ Settings n disciplines races
