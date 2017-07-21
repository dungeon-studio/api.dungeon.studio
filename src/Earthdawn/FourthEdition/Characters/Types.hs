{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Earthdawn.FourthEdition.Characters.Types
Description : Earthdawn 4th Edition Character Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Character and Collection Types.
-}
module Earthdawn.FourthEdition.Characters.Types
  ( CharacterCollection
      ( CharacterCollection
      )
  , Character
      ( Character
      , uuid
      , discipline
      , race
      )
  ) where

import Data.Map.Strict as Map (empty)
import Data.UUID (UUID)
import Network.HTTP.Media ((//))
import Network.URI (URI)

import Data.SirenJSON

-- | @application/vnd.siren+json@ compatible 'Character' collection.
data CharacterCollection = CharacterCollection URI [Character]

instance ToEntity CharacterCollection where
  toEntity (CharacterCollection u cs) = Entity
    { eClass      = [ "CharacterCollection" ]
    , eProperties = Map.empty
    , eEntities   = map (\ c -> EmbeddedRepresentation (toEntity c) ["item"]) cs
    , eLinks      = [ Link { lClass = [ "CharacterCollection" ], lRel = [ "self" ], lHref = u, lType = Just $ "application" // "vnd.siren+json", lTitle = Nothing }
                    ]
    , eActions    = []
    , eTitle      = Nothing
    }

-- | Earthdawn character representation type.
data Character = Character
  { url        :: URI
  , uuid       :: UUID
  , discipline :: URI
  , race       :: URI
  }

instance ToEntity Character where
  toEntity Character{..} = Entity
    { eClass      = [ "Character" ]
    , eProperties = Map.empty
    , eEntities   = []
    , eLinks      = [ Link { lClass = [ "Character" ], lRel = [ "self" ], lHref = url, lType = Just $ "application" // "vnd.siren+json", lTitle = Nothing }
                    , Link { lClass = [ "Discipline" ], lRel = [ "discipline" ], lHref = discipline, lType = Just $ "application" // "vnd.collection+json", lTitle = Nothing }
                    , Link { lClass = [ "Race" ], lRel = [ "race" ], lHref = race, lType = Just $ "application" // "vnd.collection+json", lTitle = Nothing }
                    ]
    , eActions    = []
    , eTitle      = Nothing
    }
