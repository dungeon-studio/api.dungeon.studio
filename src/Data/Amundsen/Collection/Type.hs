{-|

Copyright:   (c) 2015 Daniel Choi
             (c) 2017 Alex Brandt
Description: application/collection+json types
License:     MIT
Maintainer:  alunduil@alunduil.com
Module:      Collection.Type

Lifted and updated from Daniel Choi's original works which can be found here:
https://github.com/danchoi/collection-json.hs

-}
module Data.Amundsen.Collection.Type where

import Data.Text (Text)

data Collection = Collection
  { cVersion  :: Text
  , cHref     :: Text
  , cLinks    :: [Link]
  , cItems    :: [Item]
  , cQueries  :: [Query]
  , cTemplate :: Maybe Template
  , cError    :: Maybe Error
  } deriving Show

class FromCollection a where
  fromCollection :: Collection -> a

class ToCollection a where
  toCollection :: a -> Collection

data Link = Link
  { lHref   :: Text
  , lRel    :: Text
  , lName   :: Maybe Text
  , lRender :: Maybe Text
  , lPrompt :: Maybe Text
  } deriving Show

data Item = Item
  { iHref  :: Text
  , iData  :: [Data]
  , iLinks :: [Link]
  } deriving Show

data Data = Data
  { dName   :: Text
  , dValue  :: Maybe Text
  , dPrompt :: Maybe Text
  } deriving Show

data Query = Query
  { qHref   :: Text
  , qRel    :: Text
  , qName   :: Maybe Text
  , qPrompt :: Maybe Text
  , qData   :: [Data]
  } deriving Show

newtype Template = Template
  { tData :: [Data]
  } deriving Show

data Error = Error
  { eTitle   :: Text
  , eCode    :: Maybe Text
  , eMessage :: Maybe Text
  } deriving Show
