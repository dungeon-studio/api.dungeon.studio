{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Earthdawn.FourthEdition.Abilities.Types
Description : Earthdawn 4th Edition Abilities Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Ability and Collection Types.
-}
module Earthdawn.FourthEdition.Abilities.Types
  ( AbilityCollection
      ( AbilityCollection
      )
  , Ability
      ( Ability
      , name
      )
  ) where

import Network.URI (URI)

import Data.CollectionJSON
import Internal.URI

-- | @application/vnd.collection+json@ for 'Ability'.
data AbilityCollection = AbilityCollection URI [Ability]

instance ToCollection AbilityCollection where
  toCollection (AbilityCollection u as) = Collection
    { cVersion  = "1.0"
    , cHref     = u
    , cLinks    = []
    , cItems    = map (toItem u) as
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    }

-- | Earthdawn ability represenation type.
newtype Ability = Ability
  { name :: String
  }

toItem :: URI -> Ability -> Item
toItem u a = Item
  { iHref  = u'
  , iData  = []
  , iLinks = []
  }
  where u' = append u $ name a
