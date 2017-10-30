{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Earthdawn.FourthEdition.Characters.API
Description : HTTP API for Earthdawn 4th Edition Character Resources.
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for Earthdawn 4th Edition character resources.
-}
module Earthdawn.FourthEdition.Characters.API
  ( API
  , server
  ) where

import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Data.UUID (UUID)
import Network.URI (parseURIReference, URI)
import Servant ((:>), (:<|>) ((:<|>)), addHeader, AuthProtect, Capture, err403, err404, FormUrlEncoded, Get, Handler, Header, Headers, JSON, PostCreated, ReqBody, Server, throwError)

import Earthdawn.FourthEdition.Characters.Queries hiding (characters, create)
import Earthdawn.FourthEdition.Characters.Types
import Earthdawn.Settings
import Internal.JWT.Servant ()
import Internal.JWT.Types (Claims (Claims, scope, sub))
import Internal.Network.URI (append)
import Internal.Servant.API.ContentTypes.SirenJSON (SirenJSON)

import qualified Earthdawn.FourthEdition.Characters.Queries as C (characters, create)

-- | "Servant" API for Earthdawn 4th Edition Characters.
type API = AuthProtect "jwt" :> Get '[SirenJSON] CharacterCollection
      :<|> AuthProtect "jwt" :> ReqBody '[FormUrlEncoded, JSON] NewCharacter :> PostCreated '[SirenJSON] (Headers '[Header "Location" URI] Character)
      :<|> AuthProtect "jwt" :> Capture "character" UUID :> Get '[SirenJSON] Character

-- | "Servant" 'Server' for Earthdawn 4th Edition Characters.
server :: String -> Settings -> Server API
server b s = characters b s
        :<|> create b s
        :<|> character b s

characters :: String -> Settings -> Claims -> Handler CharacterCollection
characters u s Claims{sub = o, scope = ss} = 
  if "read:characters" `elem` ss then
    do cs <- liftIO $ C.characters (neo4j s) o
       return $ CharacterCollection u' cs
  else throwError err403
  where u' = fromJust $ parseURIReference u

create :: String -> Settings -> Claims -> NewCharacter -> Handler (Headers '[Header "Location" URI] Character)
create u s Claims{sub = o, scope = ss} n =
  if "create:characters" `elem` ss then
    do c <- liftIO $ C.create (neo4j s) o n
       let u' = append (fromJust $ parseURIReference u) $ show (uuid c)
       return $ addHeader u' $ c { url = u' }
  else throwError err403

character :: String -> Settings -> Claims -> UUID -> Handler Character
character b s Claims{sub = o, scope = ss} u =
  if "read:characters" `elem` ss then
    do c <- liftIO $ fromUUID (neo4j s) o u
       when (isNothing c) $ throwError err404
       return . (\ x -> x { url = u' }) $ fromJust c
  else throwError err403
  where u' = append (fromJust $ parseURIReference b) $ show u
