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

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust, isNothing)
import Data.UUID (UUID)
import Network.URI (parseURIReference)
import Servant

import Earthdawn.FourthEdition.Characters.Queries hiding (characters)
import Earthdawn.FourthEdition.Characters.Types
import Earthdawn.Settings
import Internal.Network.URI
import Servant.API.ContentTypes.SirenJSON

import qualified Earthdawn.FourthEdition.Characters.Queries as C (characters)

-- | "Servant" API for Earthdawn 4th Edition Characters.
type API = Get '[SirenJSON] CharacterCollection
      :<|> Capture "character" UUID :> Get '[SirenJSON] Character

-- | "Servant" 'Server' for Earthdawn 4th Edition Characters.
server :: String -> Settings -> Server API
server b s = characters b s
      :<|> character b s

characters :: String -> Settings -> Handler CharacterCollection
characters u s = 
  do cs <- liftIO . C.characters $ couch s
     return $ CharacterCollection u' cs
  where u' = fromJust $ parseURIReference u

character :: String -> Settings -> UUID -> Handler Character
character b s u =
  do c <- liftIO . fromUUID (couch s) $ u
     when (isNothing c) $ throwError err404
     return . (\ x -> x { url = u' }) $ fromJust c
  where u' = append (fromJust $ parseURIReference b) $ show u
