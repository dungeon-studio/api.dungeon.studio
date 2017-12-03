{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Internal.BoltPool.Environment
Description : BoltPool Environment Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and functions that generalize Bolt Pool environment interaction.
-}
module Internal.BoltPool.Environment
  ( Environment
      ( Environment
      , configuration
      , stripeCount
      , idleTime
      , maxResources
      )
  , toPool
  ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Bolt (BoltCfg (..), close, connect, Pipe)
import Data.Pool (createPool, Pool)
import Data.Time.Clock (NominalDiffTime)
import System.Envy ((.!=), envMaybe, FromEnv (fromEnv))

import External.Database.Bolt.Environment ()
import External.Data.Time.Clock.Envy ()

-- | Environment for Bolt Pool Parameters
data Environment = Environment
  { configuration :: BoltCfg         -- ^ Neo4j Configuration
  , stripeCount   :: Int             -- ^ Neo4j Pool Stripe Count
                                     --   environment variable: BOLT_POOL_STRIPE_COUNT
                                     --   default: 1
  , idleTime      :: NominalDiffTime -- ^ Neo4j Pool Connection Idle Time
                                     --   environment variable: BOLT_POOL_IDLE_TIME
                                     --   default: 30
  , maxResources  :: Int             -- ^ Neo4j Pool Connections per Stripe
                                     --   environment variable: BOLT_POOL_CONNECTIONS_PER_STRIPE
                                     --   default: 1
  }

instance Show Environment where
  show Environment{..} = s configuration ++
                                 "BOLT_POOL_STRIPE_COUNT=" ++ show stripeCount ++ "\n" ++
                                 "BOLT_POOL_IDLE_TIME=" ++ show idleTime ++ "\n" ++
                                 "BOLT_POOL_CONNECTIONS_PER_STRIPE=" ++ show maxResources ++ "\n"
    where s BoltCfg{..} = "BOLT_MAGIC=" ++ show magic ++ "\n" ++
                          "BOLT_VERSION=" ++ show version ++ "\n" ++
                          "BOLT_USERAGENT=" ++ show userAgent ++ "\n" ++
                          "BOLT_MAX_CHUNK_SIZE=" ++ show maxChunkSize ++ "\n" ++
                          "BOLT_SOCKET_TIMEOUT=" ++ show socketTimeout ++ "\n" ++
                          "BOLT_HOST=" ++ show host ++ "\n" ++
                          "BOLT_PORT=" ++ show port ++ "\n" ++
                          "BOLT_USERNAME=" ++ show user ++ "\n" ++
                          "BOLT_PASSWORD=" ++ show password ++ "\n"

instance FromEnv Environment where
  fromEnv = Environment
    <$> fromEnv
    <*> envMaybe "BOLT_POOL_STRIPE_COUNT"           .!= 1
    <*> envMaybe "BOLT_POOL_IDLE_TIME"              .!= 30
    <*> envMaybe "BOLT_POOL_CONNECTIONS_PER_STRIPE" .!= 1

-- | Convert 'Environment' to 'Pool' 'Pipe'.
toPool :: (MonadIO m) => Environment -> m (Pool Pipe)
toPool Environment{..} = liftIO $ createPool (connect configuration) close stripeCount idleTime maxResources
