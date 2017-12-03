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
      , neo4jEnvironment
      , jwtEnvironment
      , disciplines
      , races
      )
  )
  where

import Control.Applicative ((<|>))
import Network.URI (URI)
import System.Envy ((.!=), env, envMaybe, FromEnv (fromEnv))

import External.Network.URI.Envy ()

import qualified Internal.BoltPool.Environment as BoltPool
import qualified Internal.JWT.Environment as JWT

-- | Environment for api.dungeon.studio
data Environment = Environment
  { port        :: Int       -- ^ HTTP API port
                             --   environment variable: DUNGEON_STUDIO_PORT
                             --   default: 45753
                  
  , neo4jEnvironment :: BoltPool.Environment -- ^ BoltPool Settings.

  , jwtEnvironment :: JWT.Environment -- ^ JWT Settings.

  , disciplines :: URI       -- ^ Earthdawn Disciplines URI
                             --   environment variable: EARTHDAWN_DISCIPLINES_URI
  , races       :: URI       -- ^ Earthdawn Races URI
                             --   environment variable: EARTHDAWN_RACES_URI
  }

instance Show Environment where
  show Environment{..} = "DUNGEON_STUDIO_PORT=" ++ show port ++ "\n"
                      ++ show neo4jEnvironment
                      ++ show jwtEnvironment
                      ++ "EARTHDAWN_DISCIPLINES_URI=" ++ show disciplines ++ "\n"
                      ++ "EARTHDAWN_RACES_URI=" ++ show races ++ "\n"

instance FromEnv Environment where
  fromEnv = 
    do port <- ((<|>) <$> envMaybe "DUNGEON_STUDIO_PORT" <*> envMaybe "PORT") .!= 80

       neo4jEnvironment <- fromEnv
       jwtEnvironment   <- fromEnv

       disciplines <- env "EARTHDAWN_DISCIPLINES_URI"
       races       <- env "EARTHDAWN_RACES_URI"

       return Environment{..}
