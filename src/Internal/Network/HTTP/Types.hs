{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : Internal.Network.HTTP.Types
Description : HTTP Types Helper Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP types utility functions that don't belong anywhere else.
-}
module Internal.Network.HTTP.Types where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Text (unpack)
import Network.HTTP.Types (Method, renderStdMethod)

instance FromJSON Method where
  parseJSON = withText "Method" $ return . renderStdMethod . read . unpack

instance ToJSON Method where
  toJSON = toJSON . show
