{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Internal.Network.HTTP.Media.Tests
Description : MediaType Arbitrary Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of 'Arbitrary' instances for 'MediaType'.
-}
module Internal.Network.HTTP.Media.Tests where

import Network.HTTP.Media ((//), MediaType)
import Test.QuickCheck (Arbitrary (arbitrary), elements)

instance Arbitrary MediaType where
  arbitrary = elements [ "application" // "vnd.siren+json"
                       , "application" // "vnd.collection+json"
                       ]
