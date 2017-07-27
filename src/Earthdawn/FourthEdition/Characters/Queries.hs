{-|
Module      : Earthdawn.FourthEdition.Characters.Queries
Description : Earthdawn 4th Edition Character Queries
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for persisted character resources.
-}
module Earthdawn.FourthEdition.Characters.Queries
  ( characters
  , fromUUID
  ) where

import Database.CouchDB (CouchConn, DB, db, doc, getAllDocs, getDoc, runCouchDBWith)
import Data.Pool (Pool, withResource)
import Data.UUID (UUID)

import Earthdawn.FourthEdition.Characters.Types

-- | Retrieve all 'Character's.
characters :: Pool CouchConn -> IO [Character]
characters p = withResource p $ \ c -> do
  ds <- runCouchDBWith c $ getAllDocs charactersDB []
  return $ snd <$> ds

-- | Retrieve a 'Character' by uuid.
fromUUID :: Pool CouchConn -> UUID -> IO (Maybe Character)
fromUUID p u = withResource p $ \ c -> do
  character <- runCouchDBWith c $ getDoc charactersDB (doc $ show u)
  return $ case character of
    Just (_, _, x) -> Just x
    Nothing        -> Nothing

charactersDB :: DB
charactersDB = db "characters"
