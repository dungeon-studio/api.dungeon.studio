{-|
Module      : External.Network.URI.EnvySpec
Description : Tests for External.Network.URI.Envy
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.URI.Envy".
-}
module External.Network.URI.EnvySpec (main, spec) where

import Network.URI.Arbitrary ()
import Network.URI (URI)
import System.Envy (fromVar, toVar)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Invariant ((<=>))

import External.Network.URI.Envy ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "fromVar . toVar == Just" (fromVar . toVar <=> Just :: URI -> Bool)
