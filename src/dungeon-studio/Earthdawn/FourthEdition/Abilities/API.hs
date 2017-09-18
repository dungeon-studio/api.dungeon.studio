{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Earthdawn.FourthEdition.Abilities.API
Description : HTTP API for Earthdawn 4th Edition Ability Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for Earthdawn 4th Edition ability resources.
-}
module Earthdawn.FourthEdition.Abilities.API
  ( API
  , server
  ) where

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Network.URI (parseURIReference)
import Servant ((:>), (:<|>) ((:<|>)), Capture, Get, Handler, Server, throwError)

import qualified Data.Text as T (pack)

import Earthdawn.FourthEdition.Abilities.Queries hiding (abilities)
import Earthdawn.FourthEdition.Abilities.Types
import Errors
import Internal.Data.CollectionJSON (Error (..))
import Internal.Servant.API.ContentTypes.CollectionJSON (CollectionJSON)

import qualified Earthdawn.FourthEdition.Abilities.Queries as A (abilities)

-- | "Servant" API for Earthdawn 4th Edition Abilities.
type API = Get '[CollectionJSON] AbilityCollection
      :<|> Capture "ability" String :> Get '[CollectionJSON] AbilityCollection

-- | "Servant" "Server" for Earthdawn 4th Edition Disciplines.
server :: String -> Server API
server b = abilities b
      :<|> ability b

abilities :: String -> Handler AbilityCollection
abilities = return . flip AbilityCollection A.abilities . fromJust . parseURIReference

ability :: String -> String -> Handler AbilityCollection
ability b n =
  do when (isNothing a) $ throwError $ collection404 u e
     return $ AbilityCollection u [fromJust a]
  where a = fromName n
        u = fromJust $ parseURIReference b
        e = Error
              { eTitle   = Just . T.pack $ "Ability, " ++ n ++ ", Not Found"
              , eCode    = Nothing
              , eMessage = Nothing
              }
