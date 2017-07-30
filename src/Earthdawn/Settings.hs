{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
      ( neo4j
      )
  , settings
  , configure
  ) where

import Control.Monad.Catch (Handler (Handler))
import Control.Retry (constantDelay, limitRetries, recovering, RetryPolicy)
import Database.Bolt (Pipe, query_, run)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)

-- | Earthdawn Settings.
newtype Settings = Settings
  { neo4j :: Pool Pipe
  }

-- | Simplified 'Settings' Constructor.
settings :: Pool Pipe -> Settings
settings = Settings

-- | Startup initialization and configuration for Earthdawn.
--   TODO move this to where it's used?
--   TODO de-duplicate this compositionally?
configure :: Settings -> IO ()
configure Settings{..} = recovering p hs $ \ _ -> do
    withResource neo4j $ \ c -> run c $ query_ "RETURN 1"
    putStrLn "Neo4j check passed"
  where p :: RetryPolicy
        p = limitRetries 10 <> constantDelay 3000000
        hs = [ const $ Handler $ \ (e :: IOError) -> putStrLn ("Neo4j check failed: " ++ show e) >> return True
             ]
