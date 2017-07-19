{-# LANGUAGE DataKinds #-}
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
  ) where

import Network.Wai.Handler.Warp (run)
import Servant ((:>), Application, Proxy(Proxy), serve, Server)

import qualified Earthdawn.API as Earthdawn

-- | "Servant" API for our various games.
--
--   Implemented Games:
--
--   * "Earthdawn.API"
type DungeonStudioApi = "earthdawn" :> Earthdawn.API

-- | Sets up and runs a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main = run 45753 application -- TODO port as environment parameter

application :: Application
application = serve (Proxy :: Proxy DungeonStudioApi) server

server :: Server DungeonStudioApi
server = Earthdawn.server "/earthdawn"
