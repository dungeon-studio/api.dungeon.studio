{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Errors
Description : Error Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and functions that generalize API error handling.
-}
module Errors where

import Data.Aeson (encode)
import Network.HTTP.Types.Header (hContentType)
import Network.URI (URI)
import Servant (err404, ServantErr (errBody, errHeaders))

import Internal.Data.CollectionJSON (Collection (..), Error)

-- | 'Collection' (@application/vnd.collection+json@) item not found.
collection404 :: URI -> Error -> ServantErr
collection404 u e = err404
  { errHeaders = [ (hContentType, "application/vnd.collection+json") ]
  , errBody    = encode Collection
                   { cVersion  = "1.0"
                   , cHref     = u
                   , cLinks    = []
                   , cItems    = []
                   , cQueries  = []
                   , cTemplate = Nothing
                   , cError    = Just e
                   }
  }
