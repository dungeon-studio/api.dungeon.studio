{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Internal.Network.HTTP.Media
Description : HTTP Media Helper Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP media utility functions that don't belong anywhere else.
-}
module Internal.Network.HTTP.Media where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Network.HTTP.Media (MediaType, parseAccept)

import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.Text as T (unpack)

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \ v ->
    case parseAccept . B.pack . T.unpack $ v of
      Nothing -> fail "invalid MediaType"
      Just x  -> return x

instance ToJSON MediaType where
  toJSON = toJSON . show
