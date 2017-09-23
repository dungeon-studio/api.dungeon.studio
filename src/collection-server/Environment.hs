{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Environment
Description : Environment Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and functions that generalize environment interaction.
-}
module Environment
  ( Environment
      ( Environment
      , port
      , resourcePath
      )
  ) where

import System.Envy ((.!=), env, envMaybe, FromEnv (fromEnv))

-- | Environment for earthdawn-resources.
data Environment = Environment
  { port         :: Int      -- ^ HTTP API port
                             --   environment variable: COLLECTION_SERVER_PORT
                             --   default: 33080
  , resourcePath :: FilePath -- ^ Resource Directory Path
                             --   environment variable: COLLECTION_SERVER_RESOURCE_PATH
  }

instance Show Environment where
  show Environment{..} = "COLLECTION_SERVER_PORT=" ++ show port ++ "\n"
                      ++ "COLLECTION_SERVER_RESOURCE_PATH=" ++ show resourcePath ++ "\n"

instance FromEnv Environment where
  fromEnv = Environment
    <$> envMaybe "COLLECTION_SERVER_PORT"          .!= 33080
    <*> env      "COLLECTION_SERVER_RESOURCE_PATH"
