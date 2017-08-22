{-|
Module      : Main
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for collection-json property tests.
-}
module Main where

import Control.Applicative ((<$>))
import System.Exit (exitFailure, exitSuccess)

import qualified Internal.Data.SirenJSON.Tests

main :: IO ()
main =
  do success <- and <$> sequence [ Internal.Data.SirenJSON.Tests.runTests ]
     if success then exitSuccess else exitFailure
