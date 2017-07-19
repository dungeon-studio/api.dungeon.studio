{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Earthdawn.FourthEdition.Talents.API
Description : HTTP API for Earthdawn 4th Edition Talent Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for Earthdawn 4th Edition talent resources.
-}
module Earthdawn.FourthEdition.Talents.API
  ( API
  , server
  ) where

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Network.URI (parseURIReference)
import Servant

import qualified Data.Text as T (pack)

import Data.CollectionJSON
import Earthdawn.FourthEdition.Talents.Queries hiding (talents)
import Earthdawn.FourthEdition.Talents.Types
import Errors
import Servant.API.ContentTypes.CollectionJSON

import qualified Earthdawn.FourthEdition.Talents.Queries as TQ (talents)

-- | "Servant" API for Earthdawn 4th Edition Talents.
type API = Get '[CollectionJSON] TalentCollection
      :<|> Capture "talent" String :> Get '[CollectionJSON] TalentCollection

-- | "Servant" 'Server' for Earthdawn 4th Edition Talents.
server :: String -> Server API
server b = talents b
      :<|> talent b

talents :: String -> Handler TalentCollection
talents = return . flip TalentCollection TQ.talents . fromJust . parseURIReference

talent :: String -> String -> Handler TalentCollection
talent b n =
  do when (isNothing t) $ throwError $ collection404 u e
     return $ TalentCollection u [fromJust t]
  where u = fromJust $ parseURIReference b
        t = fromName n
        e = Error
              { eTitle   = Just . T.pack $ "Talent, " ++ n ++ ", Not Found"
              , eCode    = Nothing
              , eMessage = Nothing
              }
