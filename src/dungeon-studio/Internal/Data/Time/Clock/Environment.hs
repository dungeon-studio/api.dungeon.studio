{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Internal.Data.Time.Clock.Environment
Description : Environment Parsing for Data.Time.Clock
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Functions that allow Data.Time.Clock types to be read from the environment.
-}
module Internal.Data.Time.Clock.Environment where

import Data.Time.Clock (NominalDiffTime)
import System.Envy (Var (toVar, fromVar))
import Text.Read (readMaybe)

instance Var NominalDiffTime where
  toVar   = show
  fromVar = fmap (realToFrac :: Int -> NominalDiffTime) . readMaybe
