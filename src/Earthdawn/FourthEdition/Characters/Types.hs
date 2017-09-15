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
      , url
      , characters
      , disciplines
      , races
      )
  , Character
      ( Character
      , cURL
      , cUUID
      , cDiscipline
      , cRace
      )
  , NewCharacter
      ( NewCharacter
      , nDiscipline
      , nRace
      )
  ) where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Data.UUID (UUID)
import Network.HTTP.Media ((//))
import Network.HTTP.Types (methodPost)
import Network.URI (URI)
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

import qualified Data.Map.Strict as Map (empty)

import Internal.Data.SirenJSON (Action (..), Entity (..), Field (..), Link (..), SubEntity (EmbeddedRepresentation), ToEntity (toEntity), InputType (URL))
import Internal.Network.URI (append)

-- | @application/vnd.siren+json@ compatible 'Character' collection.
data CharacterCollection = CharacterCollection
  { url         :: URI
  , characters  :: [Character]
  , disciplines :: URI
  , races       :: URI
  }

instance ToEntity CharacterCollection where
  toEntity CharacterCollection{..} = Entity
    { eClass      = [ "CharacterCollection", "Earthdawn" ]
    , eProperties = Map.empty
    , eEntities   = map (\ c -> EmbeddedRepresentation (toEntity $ c { cURL = url `append` show (cUUID c) }) ["item"]) characters
    , eLinks      = [ Link { lClass = [ "CharacterCollection" ], lRel = [ "self" ], lHref = url, lType = Just $ "application" // "vnd.siren+json", lTitle = Nothing }
                    , Link { lClass = [ "DisciplineCollection" ], lRel = [ "disciplines", "related" ], lHref = disciplines, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Disciplines" }
                    , Link { lClass = [ "RaceCollection" ], lRel = [ "races", "related" ], lHref = races, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Races" }
                    ]
    , eActions    = [ Action { aName   = "create-character"
                             , aClass  = [ "Create" ]
                             , aMethod = methodPost
                             , aHref   = url
                             , aTitle  = Just "Create Character"
                             , aType   = Just $ "application" // "json"
                             , aFields = [ Field { fName = "discipline", fClass = [], fType = URL, fValue = Nothing, fTitle = Just "Discipline" }
                                         , Field { fName = "race", fClass = [], fType = URL, fValue = Nothing, fTitle = Just "Race" }
                                         ]
                             }
                    ]
    , eTitle      = Nothing
    }

-- | Earthdawn character representation type.
data Character = Character
  { cURL        :: URI
  , cUUID       :: UUID
  , cDiscipline :: URI
  , cRace       :: URI
  }

instance ToEntity Character where
  toEntity Character{..} = Entity
    { eClass      = [ "Character", "Earthdawn" ]
    , eProperties = Map.empty
    , eEntities   = []
    , eLinks      = [ Link { lClass = [ "Character" ], lRel = [ "self" ], lHref = cURL, lType = Just $ "application" // "vnd.siren+json", lTitle = Nothing }
                    , Link { lClass = [ "Discipline" ], lRel = [ "discipline" ], lHref = cDiscipline, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Discipline" }
                    , Link { lClass = [ "Race" ], lRel = [ "race" ], lHref = cRace, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Race" }
                    ]
    , eActions    = []
    , eTitle      = Nothing
    }

-- | Earthdawn new character representation type.
data NewCharacter = NewCharacter
  { nDiscipline :: URI
  , nRace       :: URI
  }

instance FromForm NewCharacter where
  fromForm f = NewCharacter
    <$> parseUnique "discipline" f
    <*> parseUnique "race" f

instance FromJSON NewCharacter where
  parseJSON = withObject "NewCharacter" $ \ v -> do
    nDiscipline <- v .: "discipline"
    nRace       <- v .: "race"

    return NewCharacter{..}
