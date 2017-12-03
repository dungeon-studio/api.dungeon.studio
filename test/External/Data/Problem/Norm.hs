{-# LANGUAGE RecordWildCards #-}

{-|
Module      : External.Data.Problem.Norm
Description : Norm Instances for External.Data.Problem
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Norm instances for "External.Data.Problem".

Also includes a definition for the 'Norm' class until it is proven generally
useful.
-}
module External.Data.Problem.Norm
  ( Norm (..)
  ) where

import Data.Maybe (isNothing)
import Network.URI (parseURI)

import External.Data.Problem

class Norm a where
  normalize :: a -> a
  normalize = id

instance Norm Problem where
  normalize Problem{..} = Problem (if isNothing pType then parseURI "about:blank" else pType)
                                  pTitle
                                  pStatus
                                  pDetail
                                  pInstance
