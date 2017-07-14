{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Main
Description : Main Module for dungeon.studio
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Standard 'Main' module with nothing special.
-}
module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant ((:>), Application, Proxy(Proxy), serve, Server)

import qualified Earthdawn.API as Earthdawn

type DungeonStudioApi = "earthdawn" :> Earthdawn.API

-- | 'main' sets up and runs an HTTP API server.
main :: IO ()
main = run 45753 application -- TODO port as environment parameter

application :: Application
application = serve (Proxy :: Proxy DungeonStudioApi) server

server :: Server DungeonStudioApi
server = Earthdawn.server "/earthdawn"
