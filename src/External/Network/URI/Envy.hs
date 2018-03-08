{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Network.URI.Envy
Description : URI Var Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI instances for Var.
-}
module External.Network.URI.Envy where

import Network.URI (parseURIReference, URI, uriToString)
import System.Envy (Var (toVar, fromVar))

instance Var URI where
  toVar   = flip (uriToString id) ""
  fromVar = parseURIReference
