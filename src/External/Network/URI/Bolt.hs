{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Network.URI.Bolt
Description : URI Bolt Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI instances for Bolt.
-}
module External.Network.URI.Bolt where

import Database.Bolt (RecordValue (exact), Value (T))
import Data.Text (unpack)
import Network.URI (URI, parseURIReference)

instance RecordValue URI where
  exact (T t) = maybe (fail "invalid URI") return $ parseURIReference $ unpack t
  exact _     = fail "invalid URI"
