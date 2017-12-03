{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : External.Data.ProblemSpec
Description : Tests for External.Data.Problem
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Data.Problem".
-}
module External.Data.ProblemSpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.URI (uriToString)
import Test.Hspec (describe, hspec, it, shouldBe, Spec)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.Invariant ((<=>))

import External.Data.Problem
import External.Data.Problem.Arbitrary ()
import External.Data.Problem.Norm

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "application/problem+json" $
    do describe "RFC compliance (https://tools.ietf.org/html/rfc7807)" $
         it "missing type should be about:blank" $ uriToString id (fromJust $ pType $ fromJust $ decode "{}") "" `shouldBe` "about:blank"
            
       describe "properties" $ modifyMaxSize (const 25) $
         prop "decode . encode == Just . normalize" (decode . encode <=> Just . normalize :: Problem -> Bool)
