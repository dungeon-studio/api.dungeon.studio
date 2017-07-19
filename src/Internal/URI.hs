{-|
Module      : Internal.URI
Description : Internal.URI
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI utility functions that don't belond anywhere else.
-}
module Internal.URI where

import Network.URI (URI, uriPath)

append :: URI -> String -> URI
append b c = b { uriPath = uriPath b ++ "/" ++ c }
