{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Internal.Network.URI
Description : URI Helper Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI utility functions that don't belong anywhere else.
-}
module Internal.Network.URI where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Either.Utils (maybeToEither)
import Data.Text (pack, unpack)
import Network.URI (parseURIReference, URI, uriPath)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

instance FromJSON URI where
  parseJSON = withText "URI" $ \ v ->
    case parseURIReference (unpack v) of
      Nothing -> fail "invalid URI"
      Just x  -> return x

instance ToJSON URI where
  toJSON = toJSON . show

instance FromHttpApiData URI where
  parseUrlPiece = maybeToEither "invalid URI" . parseURIReference . unpack
  
instance ToHttpApiData URI where
  toUrlPiece = pack . show

-- | Add 'String' to 'URI''s 'uriPath'.
append :: URI -> String -> URI
append b c = b { uriPath = uriPath b ++ "/" ++ c }
