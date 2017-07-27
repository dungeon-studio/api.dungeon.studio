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
  , Settings
      ( port
      )
  ) where

import Control.Monad.Catch (Handler (Handler))
import Control.Monad (when)
import Control.Retry (constantDelay, limitRetries, recovering, RetryPolicy)
import Database.CouchDB (CouchConn, closeCouchConn, createCouchConnFromURI, getAllDBs, runCouchDBWith)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Pool (Pool, createPool, withResource)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Application, Proxy(Proxy), serve, Server)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import qualified Earthdawn.API as Earthdawn

-- | "Servant" API for our various games.
--
--   Implemented Games:
--
--   * "Earthdawn.API"
type DungeonStudioApi = "earthdawn" :> Earthdawn.API

-- | Standard settings record.
data Settings = Settings
  { port  :: Int            -- ^ HTTP API port
                            --   environment variable: DUNGEON_STUDIO_PORT
                            --   default: 45753
  , couch :: Pool CouchConn -- ^ Couch Connection Pool
                            --   environment variable: COUCHDB_URI
                            --   default: http://localhost:5984
  }

settings :: IO Settings
settings =
  do portR <- fromMaybe (show (45753 :: Int)) <$> lookupEnv portN
     putStrLn $ portN ++ "=" ++ portR

     let portV = readMaybe portR :: Maybe Int
     when (isNothing portV) $ fail $ portN ++ " must be an Int"

     let port = fromJust portV
     when (0 > port || port > 65535) $ fail $ portN ++ " must be `elem` [0..65535]"
     
     couch <- couchFromEnv

     return Settings{..}
  where portN = "DUNGEON_STUDIO_PORT"

couchFromEnv :: IO (Pool CouchConn)
couchFromEnv =
  do uR <- fromMaybe "http://127.0.0.1:5984" <$> lookupEnv uN
     putStrLn $ uN ++ "=" ++ uR

     let uV = parseURI uR
     when (isNothing uV) $ fail $ uN ++ " must be a valid absolute URI (e.g. http://127.0.0.1:5984)"

     tR <- fromMaybe "30" <$> lookupEnv tN
     putStrLn $ tN ++ "=" ++ tR

     let tV = readMaybe tR :: Maybe Int
     when (isNothing tV) $ fail $ tN ++ " must be an Int"

     cR <- fromMaybe "10" <$> lookupEnv cN
     putStrLn $ cN ++ "=" ++ cR

     let cV = readMaybe cR :: Maybe Int
     when (isNothing cV) $ fail $ cN ++ " must be an Int"

     createPool (createCouchConnFromURI $ fromJust uV) closeCouchConn 1 (realToFrac $ fromJust tV) (fromJust cV)
  where uN = "COUCHDB_URI"
        tN = "COUCHDB_IDLE_TIMEOUT"
        cN = "COUCHDB_CONNECTION_COUNT"

-- | Sets up and runs a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main = 
  do s <- settings

     -- TODO put recovering inside connection pool
     _ <- recovering policy [h "connecting to CouchDB failed: "] $ const $ withResource (couch s) $ flip runCouchDBWith getAllDBs
     putStrLn "connected to CouchDB"

     run (port s) $ application s
  where policy :: RetryPolicy
        policy = limitRetries 10 <> constantDelay 3000000
        h p    = const . Handler $ \ (e :: IOError) -> do
                   putStrLn $ p ++ show e
                   return True

application :: Settings -> Application
application s = serve (Proxy :: Proxy DungeonStudioApi) $ server s

server :: Settings -> Server DungeonStudioApi
server = Earthdawn.server "/earthdawn" . Earthdawn.defaultSettings . couch
