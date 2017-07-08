{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Copyright:   (c) 2015 Daniel Choi
             (c) 2017 Alex Brandt
Description: application/collection+json JSON tools
License:     MIT
Maintainer:  alunduil@alunduil.com
Module:      Collection.JSON

Lifted and updated from Daniel Choi's original works which can be found here:
https://github.com/danchoi/collection-json.hs

-}
module Collection.JSON where

import Data.Aeson hiding (Error)

import Collection.Type

instance FromJSON Collection where
  parseJSON (Object v) = Collection
    <$> v .:  "version"
    <*> v .:  "href"
    <*> v .:? "links"    .!= []
    <*> v .:? "items"    .!= []
    <*> v .:? "queries"  .!= []
    <*> v .:? "template"
    <*> v .:? "error"

instance ToJSON Collection where
  toJSON Collection{..} = object
    [ "version"  .= cVersion
    , "href"     .= cHref
    , "links"    .= cLinks
    , "items"    .= cItems
    , "queries"  .= cQueries
    , "template" .= cTemplate
    , "error"    .= cError
    ]

instance FromJSON Link where
  parseJSON (Object v) = Link
    <$> v .:  "href"
    <*> v .:  "rel"
    <*> v .:? "name"
    <*> v .:? "render"
    <*> v .:? "prompt"

instance ToJSON Link where
  toJSON Link{..} = object
    [ "href"   .= lHref
    , "rel"    .= lRel
    , "name"   .= lName
    , "render" .= lRender
    , "prompt" .= lPrompt
    ]

instance FromJSON Item where
  parseJSON (Object v) = Item
    <$> v .:  "href"
    <*> v .:? "data"  .!= []
    <*> v .:? "links" .!= []

instance ToJSON Item where
  toJSON Item{..} = object
    [ "href"  .= iHref
    , "data"  .= iData
    , "links" .= iLinks
    ]

instance FromJSON Data where
  parseJSON (Object v) = Data
    <$> v .:  "name"
    <*> v .:? "value"
    <*> v .:? "prompt"

instance ToJSON Data where
  toJSON Data{..} = object
    [ "name"   .= dName
    , "data"   .= dValue
    , "prompt" .= dPrompt
    ]

instance FromJSON Query where
  parseJSON (Object v) = Query
    <$> v .:  "href"
    <*> v .:  "rel"
    <*> v .:? "name"
    <*> v .:? "prompt"
    <*> v .:? "data"   .!= []

instance ToJSON Query where
  toJSON Query{..} = object
    [ "href"   .= qHref
    , "rel"    .= qRel
    , "name"   .= qName
    , "prompt" .= qPrompt
    , "data"   .= qData
    ]

instance FromJSON Template where
  parseJSON (Object v) = Template
    <$> v .: "data"

instance ToJSON Template where
  toJSON Template{..} = object
    [ "data" .= tData
    ]

instance FromJSON Error where
  parseJSON (Object v) = Error
    <$> v .:  "title"
    <*> v .:? "code"
    <*> v .:? "message"

instance ToJSON Error where
  toJSON Error{..} = object
    [ "title"   .= eTitle
    , "code"    .= eCode
    , "message" .= eMessage
    ]
