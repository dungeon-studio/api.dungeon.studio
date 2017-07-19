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

import Data.Char
import Data.List
import Network.URI (URI)

import qualified Data.Text as T (pack)
import qualified Data.Map.Strict as Map

import Data.CollectionJSON
import Earthdawn.FourthEdition.Talents.Types hiding (name)
import Internal.URI

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
data Discipline = Discipline
  { name    :: String
  , circles :: Map.Map Int [Talent]
  }

toItem :: URI -> Discipline -> Item
toItem u d = Item
  { iHref  = u'
  , iData  = []
  , iLinks = [Link { lHref = foldl' append u' ["circles", show i], lRel = "circle", lName = Just . T.pack $ ordinal i ++ "_circle", lRender = Nothing, lPrompt = Just . T.pack $ titleize (ordinal i) ++ " Circle" } | i <- [1..8]]
  }
  where u' = append u $ name d

titleize :: String -> String
titleize = concatMap (\ (c:cs) -> toUpper c : cs) . groupBy (\ a b -> isSpace a == isSpace b)

ordinal :: Int -> String
ordinal 1 = "first"
ordinal 2 = "second"
ordinal 3 = "third"
ordinal 4 = "fourth"
ordinal 5 = "fifth"
ordinal 6 = "sixth"
ordinal 7 = "seventh"
ordinal 8 = "eigth"
ordinal _ = error "ordinal out of range"
