{-|
Module      : Earthdawn.FourthEdition.Abilities.Queries
Description : Earthdawn 4th Edition Ability Persistence Accessors
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for abililty resources that are persisted by internal means.
-}
module Earthdawn.FourthEdition.Abilities.Queries
  ( abilities
  , fromName
  ) where

import qualified Data.Map as Map (elems, lookup)

import Earthdawn.FourthEdition.Abilities.Types
import Earthdawn.FourthEdition.Abilities.Values

abilities :: [Ability]
abilities = Map.elems abilitiesMap

fromName :: String -> Maybe Ability
fromName = flip Map.lookup abilitiesMap
