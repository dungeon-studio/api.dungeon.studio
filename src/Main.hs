{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Servant.API
import Network.Wai.Handler.Warp (run)

import qualified Earthdawn.Api as Earthdawn

type DungeonStudioApi = "earthdawn" :> Earthdawn.Api

main :: IO ()
main = run 45753 application -- TODO port as environment parameter

application :: Application
application = serve p server
              where p :: Proxy DungeonStudioApi
                    p = Proxy

server :: Server DungeonStudioApi
server = Earthdawn.server "/earthdawn" earthdawnSettings
         where earthdawnSettings = Earthdawn.defaultSettings
