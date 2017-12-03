{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Errors
Description : Error Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and functions that generalizes API error handling.
-}
module Errors where

import Data.Aeson (encode)
import Data.Text (pack)
import Network.HTTP.Types.Header (hContentType)
import Servant (ServantErr (errBody, errHeaders, errHTTPCode, errReasonPhrase))

import External.Data.Problem (Problem (..))

problem :: ServantErr -> String -> ServantErr
problem e d = e
  { errHeaders = [ (hContentType, "application/problem+json") ]
  , errBody    = encode Problem
                  { pType     = Nothing -- about:blank
                  , pTitle    = Just $ pack $ errReasonPhrase e
                  , pStatus   = Just $ errHTTPCode e
                  , pDetail   = Just $ pack d
                  , pInstance = Nothing
                  }
  }
