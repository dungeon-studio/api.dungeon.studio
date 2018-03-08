{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Database.Bolt.Environment
Description : Environment Parsing for Bolt
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Functions that allow Bolt configuration to be read from the environment.
-}
module External.Database.Bolt.Environment where

import Data.Default (def)
import Database.Bolt (BoltCfg (..))
import System.Envy ((.!=), envMaybe, FromEnv (fromEnv))

instance FromEnv BoltCfg where
  fromEnv = BoltCfg
    <$> envMaybe "BOLT_MAGIC"          .!= magic def
    <*> envMaybe "BOLT_VERSION"        .!= version def
    <*> envMaybe "BOLT_USERAGENT"      .!= userAgent def
    <*> envMaybe "BOLT_MAX_CHUNK_SIZE" .!= maxChunkSize def
    <*> envMaybe "BOLT_SOCKET_TIMEOUT" .!= socketTimeout def
    <*> envMaybe "BOLT_HOST"           .!= host def
    <*> envMaybe "BOLT_PORT"           .!= port def
    <*> envMaybe "BOLT_USERNAME"       .!= user def
    <*> envMaybe "BOLT_PASSWORD"       .!= password def
    <*> envMaybe "BOLT_SECURE"         .!= secure def
