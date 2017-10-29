{-|
Module      : Internal.Docker.Compose
Description : Haskell Bindings for Docker Compose
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Haskell bindings for interacting with @docker-compose@.
-}
module Internal.Docker.Compose
  ( Service
  , found
  , services
  , up
  , down
  ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createProcess, CreateProcess, readCreateProcess, readCreateProcessWithExitCode, shell)

type Service = String

found :: MonadIO m => m Bool
found =
  do (s, _, _) <- liftIO $ readCreateProcessWithExitCode compose ""
     return $ s == ExitSuccess
  where compose = shell "which docker-compose"

services :: MonadIO m => m [Service]
services =
  do ss <- liftIO $ readCreateProcess compose ""
     return $ map trim $ lines ss
  where compose = shell "docker-compose config --services"
        trim = dropWhileEnd isSpace . dropWhile isSpace

up :: MonadIO m => Service -> m ()
up = call . shell . (++) "docker-compose up --build --no-color -d "

down :: MonadIO m => m ()
down = call $ shell "docker-compose down"

call :: MonadIO m => CreateProcess -> m ()
call = liftIO . void . createProcess
