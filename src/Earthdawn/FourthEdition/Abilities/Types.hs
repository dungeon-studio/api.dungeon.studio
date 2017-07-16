{-# LANGUAGE OverloadedStrings #-}

module Earthdawn.FourthEdition.Abilities.Types
  ( AbilityCollection
      ( AbilityCollection
      )
  , Ability
      ( Ability
      , name
      )
  ) where

import Network.URI (URI, uriPath)

import Data.CollectionJSON

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
        append :: URI -> String -> URI  -- TODO Move somewhere sensible.
        append b c = b { uriPath = uriPath b ++ "/" ++ c }
