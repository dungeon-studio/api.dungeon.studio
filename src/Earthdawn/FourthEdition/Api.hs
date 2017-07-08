{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Earthdawn.FourthEdition.Api
  ( Api
  , defaultSettings
  , raceMap
  , server
  , Settings
  )
where

import Data.Maybe (fromJust)
import Data.Text (Text, append)
import Servant
import Servant.API

import qualified Data.Map.Strict as Map

import Earthdawn.FourthEdition.Race
import Servant.API.ContentTypes.Collection

type Api = "races" :> ( Get '[CollectionJSON, JSON] [Race]
                   :<|> Capture "race" Text :> Get '[CollectionJSON, JSON] Race
                      )

newtype Settings = Settings
  { raceMap :: Map.Map Text (Text -> Race)
  }

defaultSettings = Settings $ Map.fromList [(name $ r "", r) | r <- [elf, human, obsidiman, ork, troll, tSkrang, windling]]

server :: Text -> Settings -> Server Api
server u s = races rs
      :<|> race rs
           where rs = Map.map ($ Data.Text.append u "/races") $ raceMap s

races :: Map.Map Text Race -> Handler [Race]
races = return . Map.elems

race :: Map.Map Text Race -> Text -> Handler Race
race rs = return . fromJust . flip Map.lookup rs -- TODO handle Nothing
