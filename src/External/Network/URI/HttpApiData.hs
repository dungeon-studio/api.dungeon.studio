{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : External.Network.URI.HttpApiData
Description : URI FromHttpApiData and ToHttpApiData Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI instances for FromHttpApiData and ToHttpApiData.
-}
module External.Network.URI.HttpApiData where

import Data.Text (pack, unpack)
import Network.URI (parseURIReference, URI, uriToString)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

instance FromHttpApiData URI where
  parseUrlPiece = maybe (fail "invalid URI") return . parseURIReference . unpack

instance ToHttpApiData URI where
  toUrlPiece u = pack $ uriToString id u ""
