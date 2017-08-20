{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Internal.Data.SirenJSON.Tests
Description : Tests for Data.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Internal.Data.SirenJSON".
-}
module Internal.Data.SirenJSON.Tests (runTests) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Test.Invariant ((<=>))
import Test.QuickCheck (Arbitrary (arbitrary), forAllProperties, frequency, maxSize, oneof, sized, stdArgs, quickCheckWithResult)
import Test.QuickCheck.Instances ()

import Internal.Data.SirenJSON
import Internal.Network.URI.Tests ()
import Internal.Network.HTTP.Media.Tests ()

prop_id_e :: Entity -> Bool
prop_id_e = fromJust . decode . encode <=> id

prop_id_s :: SubEntity -> Bool
prop_id_s = fromJust . decode . encode <=> id

prop_id_l :: Link -> Bool
prop_id_l = fromJust . decode . encode <=> id

prop_id_a :: Action -> Bool
prop_id_a = fromJust . decode . encode <=> id

prop_id_f :: Field -> Bool
prop_id_f = fromJust . decode . encode <=> id

prop_id_i :: InputType -> Bool
prop_id_i = fromJust . decode . encode <=> id

return []
runTests :: IO Bool
runTests = $forAllProperties $ quickCheckWithResult $ stdArgs {maxSize = 50}

instance Arbitrary Entity where
  arbitrary =
    do eClass      <- arbitrary
       eProperties <- arbitrary
       eEntities   <- arbitrary
       eLinks      <- arbitrary
       eActions    <- arbitrary
       eTitle      <- arbitrary

       return Entity{..}

instance Arbitrary SubEntity where
  arbitrary = sized $ \n -> frequency [ (n, EmbeddedLink <$> arbitrary)
                                      , (1, EmbeddedRepresentation <$> arbitrary <*> arbitrary)
                                      ]

instance Arbitrary Link where
  arbitrary =
    do lClass <- arbitrary
       lRel   <- arbitrary
       lHref  <- arbitrary
       lType  <- arbitrary
       lTitle <- arbitrary

       return Link{..}

instance Arbitrary Action where
  arbitrary =
    do aName   <- arbitrary
       aClass  <- arbitrary
       aMethod <- arbitrary
       aHref   <- arbitrary
       aTitle  <- arbitrary
       aType   <- arbitrary
       aFields <- arbitrary

       return Action{..}

instance Arbitrary Field where
  arbitrary =
    do fName  <- arbitrary
       fClass <- arbitrary
       fType  <- arbitrary
       fValue <- arbitrary
       fTitle <- arbitrary

       return Field{..}

instance Arbitrary InputType where
  arbitrary = oneof $ return <$> [ Hidden
                                 , Text
                                 , Search
                                 , Tel
                                 , URL
                                 , Email
                                 , Password
                                 , DateTime
                                 , Date
                                 , Month
                                 , Week
                                 , Time
                                 , DateTimeLocal
                                 , Number
                                 , Range
                                 , Color
                                 , CheckBox
                                 , Radio
                                 , File
                                 ]
