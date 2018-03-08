{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Data.UUID.Bolt
Description : UUID Bolt Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

UUID instances for Bolt.
-}
module External.Data.UUID.Bolt where

import Database.Bolt (RecordValue (exact), Value (T))
import Data.UUID (fromText, UUID)

instance RecordValue UUID where
  exact (T t) = maybe (fail "invalid UUID") return $ fromText t
  exact _     = fail "invalid UUID"
