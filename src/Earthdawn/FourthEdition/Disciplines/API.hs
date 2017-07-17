{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Earthdawn.FourthEdition.Disciplines.API
Description : HTTP API for Earthdawn 4th Edition Discipline Resources
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for Earthdawn 4th Edition discipline resources.
-}
module Earthdawn.FourthEdition.Disciplines.API
  ( API
  , server
  ) where

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Network.URI (parseURIReference)
import Servant

import qualified Data.Text as T (pack)

import Data.CollectionJSON
import Earthdawn.FourthEdition.Disciplines.Queries hiding (disciplines)
import Earthdawn.FourthEdition.Disciplines.Types
import Errors
import Servant.API.ContentTypes.CollectionJSON

import qualified Earthdawn.FourthEdition.Disciplines.Queries as D (disciplines)

-- | "Servant" API for Earthdawn 4th Edition Disciplines.
type API = Get '[CollectionJSON] DisciplineCollection
      :<|> Capture "discipline" String :> Get '[CollectionJSON] DisciplineCollection

-- | "Servant" "Server" for Earthdawn 4th Edition Disciplines.
server :: String -> Server API
server b = disciplines b
      :<|> discipline b

disciplines :: String -> Handler DisciplineCollection
disciplines = return . flip DisciplineCollection D.disciplines . fromJust . parseURIReference

discipline :: String -> String -> Handler DisciplineCollection
discipline b n =
  do when (isNothing d) $ throwError $ collection404 u e
     return $ DisciplineCollection u [fromJust d]
  where d = fromName n
        u = fromJust $ parseURIReference b
        e = Error
              { eTitle   = Just . T.pack $ "Discipline, " ++ n ++ ", Not Found"
              , eCode    = Nothing
              , eMessage = Nothing
              }
