{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Internal.Network.URI
Description : URI Helper Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI utility functions that don't belond anywhere else.
-}
module Internal.Network.URI where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Text (unpack)
import Network.URI (URI, parseURIReference, uriPath)

instance FromJSON URI where
  parseJSON = withText "URI" $ \ v ->
    case parseURIReference (unpack v) of
      Nothing -> fail "invalid URI"
      Just x  -> return x

instance ToJSON URI where
  toJSON = toJSON . show

-- | Add 'String' to 'URI''s 'uriPath'.
append :: URI -> String -> URI
append b c = b { uriPath = uriPath b ++ "/" ++ c }
