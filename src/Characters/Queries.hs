{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Characters.Queries
Description : Character Queries
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for persisted character resources.
-}
module Characters.Queries
  ( constraints
  , CharacterException (..)
  , all
  , create
  , fromUUID
  , delete
  ) where

import Prelude hiding (all)

import Control.Monad.Catch (catchIOError, Exception, MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((<=<), void)
import Database.Bolt (at, exact, nodeProps, Value (T), Pipe, queryP, Record, run, Value)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Data.SirenJSON (Link)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import Data.UUID (UUID, toString, toText)
import Data.UUID.V4 (nextRandom)
import Network.URI (nullURI, URI, uriToString)

import qualified Data.Map.Lazy as Map (insert, Map, fromList)

import Characters.Types
import External.Data.UUID.Bolt ()
import External.Network.URI.Bolt ()

-- * Datastore Initializations

constraints :: [Text] -- TODO
constraints = [ "CREATE CONSTRAINT ON (c:Character) ASSERT c.uuid IS UNIQUE"
              , "CREATE CONSTRAINT ON (o:Owner) ASSERT o.sub IS UNIQUE"
              ]

-- * Character Query Exceptions

data CharacterException = DoesNotExist UUID
                        | DidNotCreate
                        | CharacterIOError IOError
                        | PropertyError String
  deriving (Typeable)

instance Show CharacterException where
  show (DoesNotExist u)     = toString u ++ " does not exist"
  show DidNotCreate         = "character creation failed"
  show (CharacterIOError e) = "unexpected IOError: " ++ show e
  show (PropertyError m)    = "property parsing error: " ++ m

instance Exception CharacterException

-- * Queries

-- | Retrieve all 'Character's.
all :: (MonadIO m, MonadCatch m) => Pool Pipe -> Text -> m (URI -> [Link] -> Characters)
all p o = flip Characters <$> mapM (toCharacter <=< (`at` "c")) =<< q p cypher ps
  where cypher :: Text
        cypher = "MATCH (c:Character)<-[:OWNS|:CAN_READ]-(:Owner {sub:{sub}}) " <>
                 "RETURN c"

        ps :: Map.Map Text Value
        ps = Map.fromList [("sub", T o)]

-- | Create a 'Character'.
create :: (MonadIO m, MonadCatch m) => Pool Pipe -> Text -> NewCharacter -> m (URI -> Character)
create p o n =
  do u  <- T . toText <$> liftIO nextRandom

     cs <- mapM (toCharacter <=< (`at` "c")) =<< q p cypher (Map.insert "uuid" u ps)
     case cs of
       []  -> throwM DidNotCreate
       [c] -> return (c `withURL`)
       _   -> undefined -- TODO impossible case due to LIMIT 1

  where cypher :: Text
        cypher = "MERGE (o:Owner {sub:{sub}}) " <>
                 "CREATE UNIQUE (o)-[:OWNS]->(c:Character {uuid:{uuid}, discipline:{discipline}, race:{race}}) " <>
                 "CREATE UNIQUE (o)-[:CAN_READ]->(c) " <>
                 "RETURN c " <>
                 "LIMIT 1"

        ps :: Map.Map Text Value
        ps = Map.fromList [ ("sub",        T o)
                          , ("discipline", T $ pack $ uriToString id (nDiscipline n) "")
                          , ("race",       T $ pack $ uriToString id (nRace n) "")
                          ]

-- | Retrieve a 'Character' by uuid.
fromUUID :: (MonadIO m, MonadCatch m) => Pool Pipe -> Text -> UUID -> m (URI -> Character)
fromUUID p o u =
  do cs <- mapM (toCharacter <=< (`at` "c")) =<< q p cypher ps
     case cs of
       []  -> throwM $ DoesNotExist u
       [c] -> return (c `withURL`)
       _   -> undefined -- TODO impossible case due to LIMIT 1

  where cypher :: Text
        cypher = "MATCH (c:Character {uuid:{uuid}})<-[:OWNS|:CAN_READ]-(:Owner {sub:{sub}}) " <>
                 "RETURN c " <>
                 "LIMIT 1"

        ps :: Map.Map Text Value
        ps = Map.fromList [ ("uuid", T . pack $ toString u)
                          , ("sub",  T o)
                          ]

-- | Delete a 'Character' by uuid.
delete :: (MonadIO m, MonadCatch m) => Pool Pipe -> Text -> UUID -> m ()
delete p o u = void $ q p cypher ps
  where cypher :: Text
        cypher = "MATCH (c:Character {uuid:{uuid}})<-[:OWNS]-(:Owner {sub:{sub}}) " <>
                 "DETACH DELETE c"

        ps :: Map.Map Text Value
        ps = Map.fromList [ ("uuid", T . pack $ toString u)
                          , ("sub", T o)
                          ]

q :: (MonadIO m, MonadCatch m) => Pool Pipe -> Text -> Map.Map Text Value -> m [Record]
q p c ps = liftIO (withResource p $ flip run $ queryP c ps) `catchIOError` (throwM . CharacterIOError)

toCharacter :: (MonadThrow m) => Value -> m Character
toCharacter v =
  do ps         <- nodeProps <$> exact v

     uuid       <- exact =<< (ps `at` "uuid")
     let url = nullURI
     discipline <- exact =<< (ps `at` "discipline")
     race       <- exact =<< (ps `at` "race")

     return Character{..}
