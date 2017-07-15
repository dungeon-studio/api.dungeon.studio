{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Earthdawn.FourthEdition.Races.API
Description : HTTP API for Earthdawn 4th Edition Race Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for Earthdawn 4th Edition race resources.
-}
module Earthdawn.FourthEdition.Races.API
  ( API
  , server
  ) where

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Network.URI (parseURIReference)
import Servant

import qualified Data.Text as T (pack)

import Data.CollectionJSON
import Earthdawn.FourthEdition.Abilities.Types
import Earthdawn.FourthEdition.Races.Queries
import Earthdawn.FourthEdition.Races.Types hiding (abilities)
import Errors
import Servant.API.ContentTypes.CollectionJSON

import qualified Earthdawn.FourthEdition.Races.Types as R (abilities)

type API = ( Get '[CollectionJSON] RaceCollection
        :<|> Capture "race" String :>  Get '[CollectionJSON] RaceCollection
        :<|> Capture "race" String :> "abilities" :> Get '[CollectionJSON] AbilityCollection
           )

server :: String -> Server API
server b = races b
      :<|> race b
      :<|> abilities (b ++ "/abilities")

races :: String -> Handler RaceCollection
races = return . flip RaceCollection playerRaces . fromJust . parseURIReference

race :: String -> String -> Handler RaceCollection
race b n =
  do when (isNothing r) $ throwError $ collection404 u e
     return $ RaceCollection u [fromJust r]
  where r = fromName n
        u = fromJust $ parseURIReference b
        e = Error
              { eTitle   = Just . T.pack $ "Race, " ++ n ++ ", Not Found"
              , eCode    = Just "404"
              , eMessage = Nothing
              }

abilities :: String -> String -> Handler AbilityCollection
abilities b = return . AbilityCollection (fromJust $ parseURIReference b) . R.abilities . fromJust . fromName
