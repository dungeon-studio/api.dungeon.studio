{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}

{-|
Module      : External.Data.Problem.Arbitrary
Description : Arbitrary Instances for External.Data.Problem
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Arbitrary instances for "External.Data.Problem".
-}
module External.Data.Problem.Arbitrary where

import Network.URI (uriIsAbsolute)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Instances ()

import External.Data.Problem
import External.Network.URI.Arbitrary ()

instance Arbitrary Problem where
  arbitrary = Problem <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

  shrink Problem{..} = [ Problem pType' pTitle' pStatus' pDetail' pInstance' | (pType', pTitle', pStatus', pDetail', pInstance') <- shrink (pType, pTitle, pStatus, pDetail, pInstance), maybe True uriIsAbsolute pInstance' ]
