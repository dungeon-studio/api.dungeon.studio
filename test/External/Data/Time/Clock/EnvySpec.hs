{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : External.Data.Time.Clock.EnvySpec
Description : Tests for External.Data.Time.Clock.Envy
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Data.Time.Clock.Envy".
-}
module External.Data.Time.Clock.EnvySpec (main, spec) where

import Data.AEq ((~==))
import Data.Maybe (fromJust)
import Data.Time.Clock (NominalDiffTime)
import System.Envy (fromVar, toVar)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import External.Data.Time.Clock.Envy ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "fromVar . toVar ~== Just" (\ (t :: NominalDiffTime) -> c (fromJust $ fromVar $ toVar t) ~== c t)

c :: NominalDiffTime -> Double
c = realToFrac
