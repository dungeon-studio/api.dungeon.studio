{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Earthdawn.FourthEdition.Disciplines.Types
Description : Earthdawn 4th Edition Discipline Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Discipline and Collection Types.
-}
module Earthdawn.FourthEdition.Disciplines.Types
  ( DisciplineCollection
      ( DisciplineCollection
      )
  , Discipline
      ( Discipline
      , name
      )
  ) where

import Network.URI (URI, uriPath)

import Data.CollectionJSON

-- | @application/vnd.collection+json@ for 'Discipline'.
data DisciplineCollection = DisciplineCollection URI [Discipline]

instance ToCollection DisciplineCollection where
  toCollection (DisciplineCollection u ds) = Collection
    { cVersion  = "1.0"
    , cHref     = u
    , cLinks    = []
    , cItems    = map (toItem u) ds
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    }

-- | Earthdawn discipline representation type.
newtype Discipline = Discipline
  { name :: String
  }

toItem :: URI -> Discipline -> Item
toItem u d = Item
  { iHref = u'
  , iData = []
  , iLinks = []
  }
  where u' = append u $ name d
        append :: URI -> String -> URI
        append b c = b { uriPath = uriPath b ++ "/" ++ c }
