{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Errors
Description : Error Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and functions for various APIs provided by dungeon.studio
and in various formats.
-}
module Errors where

import Data.Aeson (encode)
import Network.HTTP.Types.Header
import Network.URI (URI)
import Servant

import Data.CollectionJSON

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
