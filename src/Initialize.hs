{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Initialize
Description : Initialization of Settings
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Initialization of 'Settings'.
-}
module Initialize
  ( initialize
  ) where

import Control.Monad.Catch (Handler (Handler), MonadMask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Retry (constantDelay, limitRetries, recovering, RetryPolicy, RetryStatus)
import Database.Bolt (Pipe, query_, run)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)

import Settings

import qualified Characters.Queries as C (constraints)

-- | Startup initialization and configuration for Earthdawn.
initialize :: (MonadIO m, MonadMask m) => Settings -> m ()
initialize Settings{..} = recovering iPolicy [hNeo4j] $ const $ iNeo4j neo4j

iPolicy :: RetryPolicy
iPolicy = limitRetries 10 <> constantDelay 3000000

retryIOError :: (MonadIO m, MonadMask m) => String -> IOError -> m Bool
retryIOError m e =
  do liftIO $ putStrLn $ m ++ show e
     return True

iNeo4j :: (MonadIO m) => Pool Pipe -> m ()
iNeo4j p = liftIO $ withResource p $ \ c ->
  do run c $ query_ "RETURN 1"
     mapM_ (run c . query_) C.constraints

hNeo4j :: (MonadIO m, MonadMask m) => RetryStatus -> Handler m Bool
hNeo4j = const $ Handler $ retryIOError "neo4j initialization failed: "
