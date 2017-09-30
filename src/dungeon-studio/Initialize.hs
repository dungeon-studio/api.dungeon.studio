{-|
Module      : Initialize
Description : Intialization of dungeon.studio
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Initialization of dungeon.studio.
-}
module Initialize
  ( initialize
  ) where

import Settings

import qualified Earthdawn.Initialize as Earthdawn

initialize :: Settings -> IO ()
initialize = Earthdawn.initialize . earthdawn