{-|
Module      : Main
Description : Main Module for collection-server
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for collection-server.
-}
module Main (main) where

import Control.Monad.Extra (notM)
import Control.Monad (when)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (Proxy), serve)
import System.Directory (doesDirectoryExist)
import System.Envy (decodeEnv)

import API
import Environment

-- | Set up and run a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main = withStdoutLogger $ \ l -> do
  e <- decodeEnv :: IO (Either String Environment)
  flip (either fail) e $ \ env -> do
    print env

    let w = setPort (port env) $
            setLogger l
            defaultSettings

    notM (doesDirectoryExist $ resourcePath env) >>= flip when (fail $ resourcePath env ++ " does not exist")

    runSettings w $ application (resourcePath env)

application :: FilePath -> Application
application = serve (Proxy :: Proxy API) . server
