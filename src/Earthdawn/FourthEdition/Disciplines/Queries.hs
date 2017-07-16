{-|
Module      : Earthdawn.FourthEdition.Disciplines.Queries
Description : Earthdawn 4th Edition Discipline Persistence Accessors
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for discipline resources that are persisted by internal means.
-}
module Earthdawn.FourthEdition.Disciplines.Queries
  ( disciplines
  , fromName
  ) where

import qualified Data.Map.Strict as Map

import Earthdawn.FourthEdition.Disciplines.Types
import Earthdawn.FourthEdition.Disciplines.Values

disciplines :: [Discipline]
disciplines = Map.elems disciplinesMap

fromName :: String -> Maybe Discipline
fromName = flip Map.lookup disciplinesMap
