{-|
Module      : Internal.Network.URI
Description : Missing functions for Network.URI
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Missing functions for "Network.URI".
-}
module Internal.Network.URI
  ( addPathPart
  ) where

import Network.URI (URI, uriPath)

addPathPart :: URI -> String -> URI
addPathPart u p = u { uriPath = uriPath u ++ "/" ++ p }
