module Characters.Scopes
  ( Resource
    ( Characters
    )
  , create
  , read
  , delete
  ) where

import Prelude hiding (read)

import Data.Text (pack, Text)

data Resource = Characters

instance Show Resource where
  show Characters = "characters"

create :: Resource -> Text
create r = pack $ "create:" ++ show r

read :: Resource -> Text
read r = pack $ "read:" ++ show r

delete :: Resource -> Text
delete r = pack $ "delete:" ++ show r
