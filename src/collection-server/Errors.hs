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
import Data.CollectionJSON (Collection (..), Error (..))
import Network.HTTP.Types.Header (hContentType)
import Network.URI (URI)
import Servant (err404, err500, ServantErr (errBody, errHeaders))

import qualified Data.Text as T (pack)

-- | 'Collection' (@application/vnd.collection+json@) item not found.
collection404 :: String -> URI -> ServantErr
collection404 n = collectionError err404 e
  where e = Error
              { eTitle   = Just . T.pack $ "Element, " ++ n ++ ", Not Found"
              , eCode    = Nothing
              , eMessage = Nothing
              }

-- | 'Collection' (@application/vnd.collection+json@) internal server error.
collection500 :: String -> URI -> ServantErr
collection500 m = collectionError err500 e
  where e = Error
              { eTitle   = Just "Couldn't Read Resource"
              , eCode    = Nothing
              , eMessage = Just $ T.pack m
              }

collectionError :: ServantErr -> Error -> URI -> ServantErr
collectionError err e u = err
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
