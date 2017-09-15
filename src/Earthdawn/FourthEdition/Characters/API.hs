{-# LANGUAGE DataKinds #-}
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
import Servant ((:>), (:<|>) ((:<|>)), addHeader, Capture, err404, FormUrlEncoded, Get, Handler, Header, Headers, JSON, PostCreated, ReqBody, Server, throwError)

import Earthdawn.FourthEdition.Characters.Queries hiding (characters, create)
import Earthdawn.FourthEdition.Characters.Types
import Earthdawn.Settings
import Internal.Network.URI (append)
import Internal.Servant.API.ContentTypes.SirenJSON (SirenJSON)

import qualified Earthdawn.FourthEdition.Characters.Queries as C (characters, create)

-- | "Servant" API for Earthdawn 4th Edition Characters.
type API = Get '[SirenJSON] CharacterCollection
      :<|> ReqBody '[FormUrlEncoded, JSON] NewCharacter :> PostCreated '[SirenJSON] (Headers '[Header "Location" URI] Character)
      :<|> Capture "character" UUID :> Get '[SirenJSON] Character

-- | "Servant" 'Server' for Earthdawn 4th Edition Characters.
server :: String -> Settings -> Server API
server b s = charactersHandler b s
        :<|> create b s
        :<|> character b s

charactersHandler :: String -> Settings -> Handler CharacterCollection
charactersHandler u s = 
  do cs <- liftIO $ C.characters $ neo4j s
     return $ CharacterCollection u' cs (disciplines s) (races s)
  where u' = fromJust $ parseURIReference u

create :: String -> Settings -> NewCharacter -> Handler (Headers '[Header "Location" URI] Character)
create u s n =
  do c <- liftIO $ C.create (neo4j s) n
     let u' = append (fromJust $ parseURIReference u) $ show (cUUID c)
     return $ addHeader u' $ c { cURL = u' }

character :: String -> Settings -> UUID -> Handler Character
character b s u =
  do c <- liftIO $ fromUUID (neo4j s) u
     when (isNothing c) $ throwError err404
     return . (\ x -> x { cURL = u' }) $ fromJust c
  where u' = append (fromJust $ parseURIReference b) $ show u
