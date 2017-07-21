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
import Data.Maybe (fromJust, isNothing)
import Data.UUID (UUID)
import Network.URI (parseURIReference)
import Servant

import Earthdawn.FourthEdition.Characters.Queries hiding (characters)
import Earthdawn.FourthEdition.Characters.Types
import Servant.API.ContentTypes.SirenJSON

import qualified Earthdawn.FourthEdition.Characters.Queries as C (characters)

-- | "Servant" API for Earthdawn 4th Edition Characters.
type API = Get '[SirenJSON] CharacterCollection
      :<|> Capture "character" UUID :> Get '[SirenJSON] Character

-- | "Servant" 'Server' for Earthdawn 4th Edition Characters.
server :: String -> Server API
server b = characters b
      :<|> character b

characters :: String -> Handler CharacterCollection
characters = return . flip CharacterCollection C.characters . fromJust . parseURIReference

character :: String -> UUID -> Handler Character
character b u =
  do when (isNothing c) $ throwError err404
     return $ fromJust c
  where c = fromUUID u (fromJust $ parseURIReference b)
