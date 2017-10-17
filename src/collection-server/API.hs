{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : API
Description : HTTP API for collection-server
Copyright   : (c) Alex Brandt, 2017
License     : MIT

API for collection-server.
-}
module API
  ( API
  , server
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, isJust)
import Network.URI (parseRelativeReference, URI)
import Servant ((:>), CaptureAll, Get, Handler, Header, Server, throwError)
import System.FilePath ((</>), joinPath, splitPath, takeBaseName, takeDirectory)

import Errors
import Types

import Internal.Network.URI (append)
import Internal.Servant.API.ContentTypes.CollectionJSON (CollectionJSON)

-- | "Servant" API for static @application/vnd.collection+json@ resources.
type API = CaptureAll "path" FilePath :> Header "Host" URI :> Get '[CollectionJSON] DirectoryCollection

-- | "Servant" 'Server' for static @application/vnd.collection+json@ resources.
server :: FilePath -> Server API
server = handler

handler :: FilePath -> [FilePath] -> Maybe URI -> Handler DirectoryCollection
handler r ss h =
  do unless (r `isPrefixOf` p') $ throwError $ e404 p -- Check for directory escapes.

     c <- liftIO $ fromPath p'
     case c of
       DoesNotExist     -> throwError $ e404 p'
       (DoesNotParse m) -> throwError $ e500 m
       _                -> return $ c `withURI` u

  where p  = foldl1 (</>) ss
        p' = collapse $ r </> if takeBaseName p == "index" then takeDirectory p else p

        u  = if isJust h then fromJust h `append` p else fromJust (parseRelativeReference p) -- TODO Feels flaky.

        e404 = flip collection404 u
        e500 = flip collection500 u

collapse :: FilePath -> FilePath
collapse = joinPath . reverse . dots . reverse . splitPath
  where dots ("/..":ss) = tail ss
        dots ("/.":ss)  = ss
        dots ss         = ss
