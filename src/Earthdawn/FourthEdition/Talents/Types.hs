{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Earthdawn.FourthEdition.Talents.Types
Description : Earthdawn 4th Edition Talent Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Talents and Collection Types.
-}
module Earthdawn.FourthEdition.Talents.Types
  ( TalentCollection
      ( TalentCollection
      )
  , Talent
      ( Talent
      , name
      )
  ) where

import Network.URI (URI)

import Data.CollectionJSON
import Internal.URI

-- | @application/vnd.collection+json@ for 'Talent'.
data TalentCollection = TalentCollection URI [Talent]

instance ToCollection TalentCollection where
  toCollection (TalentCollection u ts) = Collection
    { cVersion  = "1.0"
    , cHref     = u
    , cLinks    = []
    , cItems    = map (toItem u) ts
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    }

-- | Earthdawn talent representation type.
data Talent = Talent
  { name :: String
  }

toItem :: URI -> Talent -> Item
toItem u t = Item
  { iHref  = u'
  , iData  = []
  , iLinks = []
  }
  where u' = append u $ name t
