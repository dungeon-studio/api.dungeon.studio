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
import Earthdawn.FourthEdition.Races.Queries
import Earthdawn.FourthEdition.Races.Types
import Errors
import Servant.API.ContentTypes.CollectionJSON

type API = ( Get '[CollectionJSON] RaceCollection
        :<|> Capture "race" String :> Get '[CollectionJSON] RaceCollection
           )

server :: String -> Server API
server b = races b
      :<|> race b

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
