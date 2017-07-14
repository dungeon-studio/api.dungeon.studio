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
import Network.URI (URI, parseURI, relativeTo)
import Servant

import qualified Data.Text as T (pack)

import Earthdawn.FourthEdition.Races.Queries
import Earthdawn.FourthEdition.Races.Types
import Errors
import Servant.API.ContentTypes.Collection

-- | An API type for Earthdawn 4th Edition races.
type API = Header "host" String :> ( Get '[CollectionJSON] RaceCollection
                              :<|> Capture "race" String :> Get '[CollectionJSON] RaceCollection
                                 )

-- | Constructs an Earthdawn 4th Edition races 'Servant' 'Server' given a URL path prefix.
server :: URI -> Server API
server b h = races u
        :<|> race u
             where u = b `relativeTo` (fromJust . parseURI . fromJust $ h)

races :: URI -> Handler RaceCollection
races = return . flip RaceCollection playerRaces

race :: URI -> String -> Handler RaceCollection
race u n =
  do when (isNothing r) $ throwError $ collection404 u (Just . T.pack $ "Race, " ++ n ++ ", Not Found") (Just "404") Nothing
     return $ RaceCollection u [fromJust r]
  where r = fromName n
