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
  ( characters
  , create
  , fromUUID
  ) where

import Database.Bolt (at, exact, nodeProps, Value (T), Pipe, query, queryP, run, Value)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Data.Text (Text, pack, unpack)
import Data.Traversable (traverse)
import Data.UUID (UUID, fromString, toString)
import Data.UUID.V4 (nextRandom)
import Network.URI (nullURI, parseURI)

import qualified Data.Map.Lazy as Map (Map, fromList)

import Earthdawn.FourthEdition.Characters.Types

-- | Retrieve all 'Character's.
characters :: Pool Pipe -> IO [Character]
characters p = withResource p $ \ c -> do
    ns <- run c $ query cypher >>= traverse (`at` "character")
    traverse toCharacter ns
  where cypher :: Text
        cypher = "MATCH (character:Character) " <>
                 "WHERE character:Earthdawn " <>
                 "RETURN character"

-- | Create a 'Character'.
create :: Pool Pipe -> NewCharacter -> IO Character
create p n = withResource p $ \ c -> do
    u <- nextRandom
    let ps :: Map.Map Text Value
        ps = Map.fromList [ ("uuid",       T . pack . toString $ u)
                          , ("race",       T . pack . show . nRace $ n)
                          , ("discipline", T . pack . show . nDiscipline $ n)
                          ]
    ns <- run c $ queryP cypher ps >>= traverse (`at` "character")
    toCharacter $ head ns
  where cypher :: Text
        cypher = "CREATE (character:Character:Earthdawn {uuid:{uuid}, discipline:{discipline}, race:{race}}) " <>
                 "RETURN character"
  
-- | Retrieve a 'Character' by uuid.
fromUUID :: Pool Pipe -> UUID -> IO (Maybe Character)
fromUUID p u = withResource p $ \ c -> do
    ns <- run c $ queryP cypher ps >>= traverse (`at` "character")
    return $ if length ns < 1 then Nothing else toCharacter $ head ns
  where cypher :: Text
        cypher = "MATCH (character:Character {uuid:{uuid}}) " <>
                 "RETURN character " <>
                 "LIMIT 1"

        ps :: Map.Map Text Value
        ps = Map.fromList [("uuid", T . pack $ toString u)]

toCharacter :: Monad m => Value -> m Character
toCharacter v =
  do ps         <- nodeProps <$> exact v

     uuid       <- (fromJust . fromString . unpack) <$> ((ps `at` "uuid") >>= exact)
     let url = nullURI
     discipline <- (fromJust . parseURI . unpack) <$> ((ps `at` "discipline") >>= exact)
     race       <- (fromJust . parseURI . unpack) <$> ((ps `at` "race") >>= exact)

     return Character{..}
