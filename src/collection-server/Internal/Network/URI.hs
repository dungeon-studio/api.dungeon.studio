{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Internal.Network.URI
Description : URI Helper Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI utility functions that don't belong anywhere else.
-}
module Internal.Network.URI where

import Data.Either.Utils (maybeToEither)
import Data.Text (unpack)
import Network.URI (parseURIReference, URI, uriPath)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

instance FromHttpApiData URI where
  parseUrlPiece = maybeToEither "invalid URI" . parseURIReference . unpack
  
-- | Add 'String' to 'URI''s 'uriPath'.
append :: URI -> String -> URI
append b c = b { uriPath = uriPath b ++ "/" ++ c }
