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

import Control.Lens ((.~), (&))
import Crypto.JWT (audiencePredicate, defaultJWTValidationSettings, fromURI, issuerPredicate)
import Data.Either.Combinators (whenLeft)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai (Request)
import Servant ((:>), Application, Context ((:.), EmptyContext), Proxy (Proxy), serveWithContext, Server)
import Servant.Server.Experimental.Auth (AuthHandler)
import System.Envy ((.!=), decodeEnv, envMaybe, FromEnv (fromEnv))

import Environment

import qualified Earthdawn.API as Earthdawn
import qualified Internal.Auth0 as Auth0 (Claims, Environment (audience, issuer, jwksURI), handler)

data Environment = Environment
  { port     :: Int                 -- ^ HTTP API port
                                    --   environment variable: DUNGEON_STUDIO_PORT
                                    --   default: 45753
  , bolt     :: BoltPoolEnvironment -- ^ Neo4j Configuration
  , auth0    :: Auth0.Environment   -- ^ Auth0 Configuration
  }

instance Show Environment where
  show Environment{..} = "DUNGEON_STUDIO_PORT=" ++ show port ++ "\n" ++ show bolt ++ show auth0

instance FromEnv Environment where
  fromEnv = Environment
    <$> envMaybe "DUNGEON_STUDIO_PORT" .!= 45753
    <*> fromEnv
    <*> fromEnv

newtype Settings = Settings
  { earthdawn :: Earthdawn.Settings
  }

configure :: Settings -> IO ()
configure = Earthdawn.configure . earthdawn

context :: Environment -> Context (AuthHandler Request Auth0.Claims ': '[])
context e = Auth0.handler u s :. EmptyContext
  where u = Auth0.jwksURI $ auth0 e
        s = defaultJWTValidationSettings
              & audiencePredicate .~ (== (fromURI . Auth0.audience . auth0 $ e))
              & issuerPredicate   .~ (== (fromURI . Auth0.issuer . auth0 $ e ))

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

     let (Right e') = e
     print e'

     b <- toPool $ bolt e'

     let s = Settings
               { earthdawn = Earthdawn.settings b
               }

     configure s

     withStdoutLogger $ \ l -> do
       let ws = setPort (port e') .  setLogger l $ defaultSettings
       runSettings ws $ application s e'

application :: Settings -> Environment -> Application
application s e = serveWithContext (Proxy :: Proxy DungeonStudioApi) (context e) $ server s

server :: Settings -> Server DungeonStudioApi
server = Earthdawn.server "/earthdawn" . earthdawn
