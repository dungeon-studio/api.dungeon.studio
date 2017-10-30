{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Environment
Description : Environment Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and functions that generalize environment interaction.
-}
module Environment
  ( Environment
      ( Environment
      , port
      , neo4j
      , disciplines
      , races
      , jwtSettings
      )
  )
  where

import Control.Monad.Trans (liftIO)
import Database.Bolt (BoltCfg (..), close, connect, Pipe)
import Data.Maybe (fromJust)
import Data.Pool (createPool, Pool)
import Data.Time.Clock (NominalDiffTime)
import Network.URI (parseURI, URI)
import System.Envy ((.!=), envMaybe, FromEnv (fromEnv))

import Internal.Database.Bolt.Environment ()
import Internal.Data.Time.Clock.Environment ()
import Internal.Network.URI ()

import qualified Internal.JWT.Environment as JWT

-- | Environment for api.dungeon.studio
data Environment = Environment
  { port        :: Int       -- ^ HTTP API port
                             --   environment variable: DUNGEON_STUDIO_PORT
                             --   default: 45753
  , neo4j       :: Pool Pipe -- ^ Neo4j Pool connection
  , disciplines :: URI       -- ^ Earthdawn Disciplines URI
                             --   environment variable: EARTHDAWN_DISCIPLINES_URI
                             --   default: http://static.dungeon.studio/earthdawn/4e/disciplines
                             --   TODO Change this to a system hosted under earthdawn.dungeon.studio
  , races       :: URI       -- ^ Earthdawn Races URI
                             --   environment variable: EARTHDAWN_RACES_URI
                             --   default: http://static.dungeon.studio/earthdawn/4e/races
                  
  , jwtSettings   :: JWT.Environment   -- ^ JWT Settings.  TODO generalize
                                         --   environment settings

  , neo4jSettings :: BoltPoolEnvironment -- Placeholder for presentation.
  }

instance Show Environment where
  show Environment{..} = "DUNGEON_STUDIO_PORT=" ++ show port ++ "\n"
                      ++ show neo4jSettings
                      ++ "EARTHDAWN_DISCIPLINES_URI=" ++ show disciplines ++ "\n"
                      ++ "EARTHDAWN_RACES_URI=" ++ show races ++ "\n"
                      ++ show jwtSettings

instance FromEnv Environment where
  fromEnv = 
    do port          <- envMaybe "DUNGEON_STUDIO_PORT"       .!= 45753

       disciplines'  <- envMaybe "EARTHDAWN_DISCIPLINES_URI" .!= "http://static.dungeon.studio/earthdawn/4e/disciplines"
       let disciplines = fromJust $ parseURI disciplines'
      
       races'        <- envMaybe "EARTHDAWN_RACES_URI"       .!= "http://static.dungeon.studio/earthdawn/4e/races"
       let races = fromJust $ parseURI races'

       jwtSettings   <- fromEnv

       neo4jSettings <- fromEnv
       neo4j         <- liftIO $ toPool neo4jSettings

       return Environment{..}

-- | Environment for Bolt Pool Parameters
data BoltPoolEnvironment = BoltPoolEnvironment
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

instance Show BoltPoolEnvironment where
  show BoltPoolEnvironment{..} = s configuration ++
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

instance FromEnv BoltPoolEnvironment where
  fromEnv = BoltPoolEnvironment
    <$> fromEnv
    <*> envMaybe "BOLT_POOL_STRIPE_COUNT"           .!= 1
    <*> envMaybe "BOLT_POOL_IDLE_TIME"              .!= 30
    <*> envMaybe "BOLT_POOL_CONNECTIONS_PER_STRIPE" .!= 1

-- | Convert 'BoltPoolEnvironment' to 'Pool' 'Pipe'.
toPool :: BoltPoolEnvironment -> IO (Pool Pipe)
toPool BoltPoolEnvironment{..} = createPool (connect configuration) close stripeCount idleTime maxResources
