{-|
Module      : Internal.Test.Contexts.Docker.Compose
Description : Testing Context for Docker Compose
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Testing context for docker-compose.
-}
module Internal.Test.Contexts.Docker.Compose
  ( withCompose
  ) where

import Control.Monad.Catch (bracket, Exception, MonadThrow, throwM)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (unless)
import Test.Hspec (ActionWith)

import Internal.Docker.Compose (down, found, Service, services, up)

type Port = String

data ComposeException = ComposeNotFound
                      | ServiceNotFound String

instance Show ComposeException where
  show ComposeNotFound     = "docker-compose not found in shell ${PATH}"
  show (ServiceNotFound s) = "service, " ++ s ++ ", not found in docker-compose.yml"

instance Exception ComposeException

withCompose :: (MonadIO m, MonadThrow m) => Service -> m Bool ->  ActionWith Port -> m ()
withCompose s c a =
  do unlessM found $ throwM ComposeNotFound

     ss <- services
     unless (s `elem` ss) $ throwM $ ServiceNotFound s

     liftIO $ bracket (up' s c) (const down) a

up' :: MonadIO m => Service -> m Bool -> m Port
up' s c =
  do up s
     recovering p hs \ _ -> unlessM c

     -- up service
     -- wait for service
     -- return service port
  where p :: RetryPolicy
        p = limitRetries
