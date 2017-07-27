{-|
Module      : Earthdawn.Settings
Description : Settings for Earthdawn APIs
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Settings for Earthdawn APIs.
-}
module Earthdawn.Settings
  ( Settings
      ( couch
      )
  , defaultSettings
  ) where

import Database.CouchDB (CouchConn)
import Data.Pool (Pool)

-- | Standard settings record.
newtype Settings = Settings
  { couch :: Pool CouchConn
  }

-- | Pre-populated settings object constructor.
--
--   Sets some but not all defaults.  Items not handled by defaults are taken as
--   arguments currently.
defaultSettings :: Pool CouchConn -> Settings
defaultSettings = Settings
