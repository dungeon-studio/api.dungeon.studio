{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Earthdawn.FourthEdition.Races.API
  ( API
  , server
  ) where

import Control.Monad (when)
import Data.Aeson (encode)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import Network.HTTP.Types.Header (hContentType)
import Numeric.Natural (Natural)
import Servant -- TODO ((:<|>), (:>), Capture, err404, errBody, errHeaders, Get, Handler, Server, throwError)

import qualified Data.Text as T (concat, pack)

import Data.Amundsen.Collection
import Earthdawn.FourthEdition.Races.Types
import Earthdawn.FourthEdition.Races.Queries
import Servant.API.ContentTypes.Collection

type API = Get '[CollectionJSON] [Race]
      :<|> Capture "race" Text :> Get '[CollectionJSON] Race

server :: Text -> Server API
server b = races b
      :<|> race b

races :: Text -> Handler [Race]
races b = return $ map (\ r -> r { collectionURL = b}) playerRaces

race :: Text -> Text -> Handler Race
race b n =
  do when (isNothing r) (throwError raceNotFound)
     return $ (fromJust r) { collectionURL = b }
  where r = fromName n
        raceNotFound = err404
          { errHeaders = [ (hContentType, "application/vnd.collection+json") ]
          , errBody    = encode $ collectionError b (T.concat ["Race, ", n, ", Not Found"]) (Just "404") (Just "Refer to the race collection for valid races.")
          }
