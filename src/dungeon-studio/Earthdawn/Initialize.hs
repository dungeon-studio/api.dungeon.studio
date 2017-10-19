{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Earthdawn.Initialize
Description : Initialization of Earthdawn Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Initialization of Earthdawn resources.
-}
module Earthdawn.Initialize
  ( initialize
  ) where

import Control.Monad.Catch (Handler (Handler))
import Control.Retry (constantDelay, limitRetries, recovering, RetryPolicy)
import Database.Bolt (query_, run)
import Data.Monoid ((<>))
import Data.Pool (withResource)

import Earthdawn.FourthEdition.Characters.Queries (constraints)
import Earthdawn.Settings

-- | Startup initialization and configuration for Earthdawn.
initialize :: Settings -> IO ()
initialize Settings{..} =
  recovering p hs $ \ _ -> withResource neo4j $ \ c -> do
    run c $ query_ "RETURN 1"
    mapM_ (run c . query_) constraints 
  where p  :: RetryPolicy
        p  = limitRetries 10 <> constantDelay 3000000

        hs = [ const $ Handler $ \ (e :: IOError) -> putStrLn ("Neo4j check failed: " ++ show e) >> return True ]
