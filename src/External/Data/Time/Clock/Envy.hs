{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Data.Time.Clock.Envy
Description : Environment Parsing for Data.Time.Clock
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Functions that allow Data.Time.Clock types to be read from the environment.
-}
module External.Data.Time.Clock.Envy where

import Data.Time.Clock (NominalDiffTime)
import System.Envy (Var (toVar, fromVar))
import Text.Read (readMaybe)

instance Var NominalDiffTime where
  toVar   = init . show
  fromVar = fmap (realToFrac :: Double -> NominalDiffTime) . readMaybe
