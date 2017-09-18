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

import Data.Text (Text)
import Servant ((:>), CaptureAll, Get, Handler, Header, Server)

import qualified Data.Text as T (pack)

import Internal.Data.CollectionJSON (Error (Error, eCode, eMessage, eTitle))
import Internal.Servant.API.ContentTypes.CollectionJSON (CollectionJSON)

-- | "Servant" API for static @application/vnd.collection+json@ resources.
type API = CaptureAll "path" FilePath :> Header "Host" Text :> Get '[CollectionJSON] Something

-- | "Servant" 'Server' for static @application/vnd.collection+json@ resources.
server :: FilePath -> Server API
server = handler

handler :: FilePath -> [FilePath] -> Text -> Handler Something
handler r ss h =
  do when (commonPrefix [r, p] /= r) $ throwError $ collection404 u e -- Check for directory escapes.

     return $ toCollection u $ fromPath (p <.> "yaml")
  where p = collapse $ r </> joinPath ss
        u = h `append` p
        e = Error
              { eTitle   = Just . T.pack $ "Element, " ++ takeBaseName p ++ ", Not Found"
              , eCode    = Nothing
              , eMessage = Nothing
              }

fromFile :: FilePath -> IO (Maybe Something)
fromFile = decodeFile

fromDirectory :: FilePath -> IO (Maybe Something)
fromDirectory p =
  do fs <- listDirectory p
     mapM decodeFile fs
