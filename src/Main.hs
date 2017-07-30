{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Main
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for dungeon.studio.
-}
module Main
  ( DungeonStudioApi
  , main
  , Environment
      ( port
      , bolt
      )
  ) where

import Data.Either.Combinators (whenLeft, whenRight)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Application, Proxy(Proxy), serve, Server)
import System.Envy ((.!=), decodeEnv, envMaybe, FromEnv (fromEnv))

import Environment

import qualified Earthdawn.API as Earthdawn

data Environment = Environment
  { port :: Int                 -- ^ HTTP API port
                                --   environment variable: DUNGEON_STUDIO_PORT
                                --   default: 45753
  , bolt :: BoltPoolEnvironment -- ^ Neo4j Configuration
  }

instance Show Environment where
  show Environment{..} = "DUNGEON_STUDIO_PORT=" ++ show port ++ "\n" ++ show bolt

instance FromEnv Environment where
  fromEnv = Environment
    <$> envMaybe "DUNGEON_STUDIO_PORT" .!= 45753
    <*> fromEnv

newtype Settings = Settings
  { earthdawn :: Earthdawn.Settings
  }

configure :: Settings -> IO ()
configure = Earthdawn.configure . earthdawn

-- | "Servant" API for our various games.
--
--   Implemented Games:
--
--   * "Earthdawn.API"
type DungeonStudioApi = "earthdawn" :> Earthdawn.API

-- | Sets up and runs a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main = 
  do e <- decodeEnv :: IO (Either String Environment)
     whenLeft e fail
     whenRight e $ \ e' -> do
       print e'

       b <- toPool $ bolt e'

       let s = Settings
                 { earthdawn = Earthdawn.settings b
                 }

       configure s

       run (port e') $ application s

application :: Settings -> Application
application s = serve (Proxy :: Proxy DungeonStudioApi) $ server s

server :: Settings -> Server DungeonStudioApi
server = Earthdawn.server "/earthdawn" . earthdawn
