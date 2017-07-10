{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant ((:>), Application, Proxy(Proxy), serve, Server)
import Network.Wai.Handler.Warp (run)

import qualified Earthdawn.API as Earthdawn

type DungeonStudioApi = "earthdawn" :> Earthdawn.API

main :: IO ()
main = run 45753 application -- TODO port as environment parameter

application :: Application
application = serve (Proxy :: Proxy DungeonStudioApi) server

server :: Server DungeonStudioApi
server = Earthdawn.server "/earthdawn"
