{-# LANGUAGE DataKinds #-}
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

import Earthdawn.FourthEdition.Abilities.Types
import Earthdawn.FourthEdition.Races.Queries hiding (races)
import Earthdawn.FourthEdition.Races.Types hiding (abilities)
import Errors
import Internal.Data.CollectionJSON (Error (..))
import Internal.Servant.API.ContentTypes.CollectionJSON (CollectionJSON)

import qualified Earthdawn.FourthEdition.Races.Queries as RQ (races)
import qualified Earthdawn.FourthEdition.Races.Types as RT (abilities)

-- | "Servant" API for Earthdawn 4th Edition Races.
type API = Get '[CollectionJSON] RaceCollection
      :<|> Capture "race" String :>  Get '[CollectionJSON] RaceCollection
      :<|> Capture "race" String :> "abilities" :> Get '[CollectionJSON] AbilityCollection

-- | "Servant" "Server" for Earthdawn 4th Edition Races.
server :: String -> Server API
server b = races b
      :<|> race b
      :<|> abilities (((b ++ "/") ++) . (++ "/abilities"))

races :: String -> Handler RaceCollection
races = return . flip RaceCollection RQ.races . fromJust . parseURIReference

race :: String -> String -> Handler RaceCollection
race b n =
  do when (isNothing r) $ throwError $ collection404 u e
     return $ RaceCollection u [fromJust r]
  where r = fromName n
        u = fromJust $ parseURIReference b
        e = Error
              { eTitle   = Just . T.pack $ "Race, " ++ n ++ ", Not Found"
              , eCode    = Nothing
              , eMessage = Nothing
              }

abilities :: (String -> String) -> String -> Handler AbilityCollection
abilities b n = return . AbilityCollection (fromJust $ parseURIReference $ b n) . RT.abilities . fromJust . fromName $ n
