{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Main
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for dungeon.studio.
-}
module Main (main) where

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (Proxy), serve)
import System.Envy (decodeEnv)

import API
import Environment
import Initialize
import Settings

-- | Sets up and runs a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main = withStdoutLogger $ \ l -> do
  e <- decodeEnv :: IO (Either String Environment)
  flip (either fail) e $ \ env -> do
    print env

    let s = settings env
    initialize s

    let w = setPort (port env) $
            setLogger l
            defaultSettings

    runSettings w $ application s

application :: Settings -> Application
application = serve (Proxy :: Proxy API) . server
