{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Characters.Types
Description : Character Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Character and Collection Types.
-}
module Characters.Types
  ( Characters
      ( Characters
      )
  , Character
      ( Character
      , url
      , uuid
      , discipline
      , race
      )
  , withURL
  , NewCharacter
      ( NewCharacter
      , nDiscipline
      , nRace
      )
  ) where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Data.SirenJSON (Action (..), Entity (..), Field (..), Link (..), SubEntity (EmbeddedRepresentation), ToEntity (toEntity), InputType (URL))
import Data.UUID (toString, UUID)
import Network.HTTP.Media ((//))
import Network.HTTP.Types.Method (StdMethod (POST))
import Network.URI (URI)
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

import qualified Data.Map.Strict as Map (empty)

import External.Network.URI.HttpApiData ()
import Internal.Network.URI (addPathPart)

-- | @application/vnd.siren+json@ compatible 'Character' collection.
data Characters = Characters URI [Character] [Link]

instance ToEntity Characters where
  toEntity (Characters u cs ls) = Entity
    { eClass      = [ "Characters" ]
    , eProperties = Map.empty
    , eEntities   = map (toSubEntity u) cs
    , eLinks      = Link { lClass = [ "Characters" ], lRel = [ "self" ], lHref = u, lType = Just $ "application" // "vnd.siren+json", lTitle = Nothing } : ls
    , eActions    = [ Action { aName   = "create-character"
                             , aClass  = [ "Create" ]
                             , aMethod = Just POST
                             , aHref   = u
                             , aTitle  = Just "Create Character"
                             , aType   = Just $ "application" // "json"
                             , aFields = [ Field { fName = "discipline", fClass = [], fType = Just URL, fValue = Nothing, fTitle = Just "Discipline" }
                                         , Field { fName = "race", fClass = [], fType = Just URL, fValue = Nothing, fTitle = Just "Race" }
                                         ]
                             }
                    ]
    , eTitle      = Nothing
    }

toSubEntity :: URI -> Character -> SubEntity
toSubEntity u c = EmbeddedRepresentation (toEntity (c `withURL` u)) ["item"]

-- | Earthdawn character representation type.
data Character = Character
  { url        :: URI
  , uuid       :: UUID
  , discipline :: URI
  , race       :: URI
  }

-- TODO Lenses?
withURL :: Character -> URI -> Character
withURL c u = c { url = u `addPathPart` toString (uuid c) }

instance ToEntity Character where
  toEntity Character{..} = Entity
    { eClass      = [ "Character" ]
    , eProperties = Map.empty
    , eEntities   = []
    , eLinks      = [ Link { lClass = [ "Character" ], lRel = [ "self" ], lHref = url, lType = Just $ "application" // "vnd.siren+json", lTitle = Nothing }
                    , Link { lClass = [ "Discipline" ], lRel = [ "discipline" ], lHref = discipline, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Discipline" }
                    , Link { lClass = [ "Race" ], lRel = [ "race" ], lHref = race, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Race" }
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
  fromForm f = NewCharacter <$> parseUnique "discipline" f
                            <*> parseUnique "race" f

instance FromJSON NewCharacter where
  parseJSON = withObject "NewCharacter" $ \ v ->
    do nDiscipline <- v .: "discipline"
       nRace       <- v .: "race"

       return NewCharacter{..}
