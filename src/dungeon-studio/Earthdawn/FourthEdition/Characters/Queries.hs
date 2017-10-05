{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Earthdawn.FourthEdition.Characters.Queries
Description : Earthdawn 4th Edition Character Queries
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for persisted character resources.
-}
module Earthdawn.FourthEdition.Characters.Queries
  ( constraints
  , characters
  , create
  , fromUUID
  ) where

import Database.Bolt (at, exact, nodeProps, Value (T), Pipe, queryP, run, Value)
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Data.Text (Text, pack, unpack)
import Data.Traversable (traverse)
import Data.UUID (UUID, fromString, toString)
import Data.UUID.V4 (nextRandom)
import Network.URI (nullURI, parseURI)

import qualified Data.Map.Lazy as Map (insert, Map, fromList)

import Earthdawn.FourthEdition.Characters.Types

constraints :: [Text]
constraints = [ "CREATE CONSTRAINT ON (c:Character) ASSERT c.uuid IS UNIQUE"
              , "CREATE CONSTRAINT ON (o:Owner) ASSERT o.sub IS UNIQUE"
              ]

-- | Retrieve all 'Character's.
characters :: Pool Pipe -> Text -> IO [Character]
characters p o = withResource p $ \c -> run c $ queryP cypher ps >>= traverse (`at` "c") >>= traverse toCharacter
  where cypher :: Text
        cypher = "MATCH (c:Earthdawn:Character)<-[:OWNS|:CAN_READ]-(:Owner {sub:{sub}}) " <>
                 "RETURN c"

        ps :: Map.Map Text Value
        ps = Map.fromList [("sub", T o)]

-- | Create a 'Character'.
create :: Pool Pipe -> Text -> NewCharacter -> IO Character
create p o n =
  do u <- T . pack . toString <$> nextRandom
     withResource p $ \c -> run c $ queryP cypher (Map.insert "uuid" u ps) >>= traverse (`at` "c") >>= toCharacter . head
  where cypher :: Text
        cypher = "MERGE (o:Owner {sub:{sub}}) " <>
                 "CREATE UNIQUE (o)-[:OWNS]->(c:Earthdawn:Character {uuid:{uuid}, discipline:{discipline}, race:{race}}) " <>
                 "CREATE UNIQUE (o)-[:CAN_READ]->(c) " <>
                 "RETURN c"

        ps :: Map.Map Text Value
        ps = Map.fromList [ ("discipline", T . pack . show . nDiscipline $ n)
                          , ("race",       T . pack . show . nRace $ n)
                          , ("sub",        T o)
                          ]

-- | Retrieve a 'Character' by uuid.
fromUUID :: Pool Pipe -> Text -> UUID -> IO (Maybe Character)
fromUUID p o u = withResource p $ \c -> run c $ queryP cypher ps >>= traverse (`at` "c") >>= fmap listToMaybe . traverse toCharacter
  where cypher :: Text
        cypher = "MATCH (c:Character {uuid:{uuid}})<-[:OWNS|:CAN_READ]-(:Owner {sub:{sub}}) " <>
                 "RETURN c " <>
                 "LIMIT 1"

        ps :: Map.Map Text Value
        ps = Map.fromList [ ("uuid", T . pack $ toString u)
                          , ("sub",  T o)
                          ]

toCharacter :: Monad m => Value -> m Character
toCharacter v =
  do ps         <- nodeProps <$> exact v

     uuid       <- (fromJust . fromString . unpack) <$> ((ps `at` "uuid") >>= exact)
     let url = nullURI
     discipline <- (fromJust . parseURI . unpack) <$> ((ps `at` "discipline") >>= exact)
     race       <- (fromJust . parseURI . unpack) <$> ((ps `at` "race") >>= exact)

     return Character{..}
