{-|
Module      : External.Network.URI.HttpApiDataSpec
Description : Tests for External.Network.URI.HttpApiData
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.URI.HttpApiData".
-}
module External.Network.URI.HttpApiDataSpec (main, spec) where

import Network.URI.Arbitrary ()
import Network.URI (URI)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Invariant ((<=>))
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

import External.Network.URI.HttpApiData ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "parseUrlPiece . toUrlPiece == Right" (parseUrlPiece . toUrlPiece <=> Right :: URI -> Bool)
