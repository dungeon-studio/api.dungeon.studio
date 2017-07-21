{-|
Module      : Earthdawn.FourthEdition.Characters.Queries
Description : Earthdawn 4th Edition Character Queries
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Accessors for persisted character resources.
-}
module Earthdawn.FourthEdition.Characters.Queries
  ( characters
  , fromUUID
  ) where

import Data.UUID (UUID)
import Network.URI (URI)

import Earthdawn.FourthEdition.Characters.Types

-- | Retrieve all 'Character's.
characters :: [Character]
characters = undefined

-- | Retrieve a 'Character' by uuid.
fromUUID :: UUID -> URI -> Maybe Character
fromUUID = undefined
