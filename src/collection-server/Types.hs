{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Types
Description : Types for collection-server
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Types for collection-server.
-}
module Types
  ( DirectoryCollection
      ( DirectoryCollection
      , DoesNotParse
      , DoesNotExist
      )
  , withURI
  , fromPath
  ) where

import Control.Monad.Extra (ifM)
import Data.CollectionJSON (Collection (..), Item, ToCollection (toCollection))
import Data.Either (partitionEithers)
import Data.Yaml (decodeFileEither)
import Network.URI (nullURI, URI)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), (<.>))

-- | Convenience type for converting files and directories to 'CollectionJSON'
--   for "Servant".
data DirectoryCollection = DirectoryCollection URI [Item]
                         | DoesNotParse String
                         | DoesNotExist

instance ToCollection DirectoryCollection where
  toCollection (DirectoryCollection u xs) = Collection
    { cVersion  = "1.0"
    , cHref     = u
    , cLinks    = []
    , cItems    = xs
    , cQueries  = []
    , cTemplate = Nothing
    , cError    = Nothing
    }
  toCollection _ = error "invalid DirectoryCollection for ToCollection"

-- | Set URI to passed value.
withURI :: DirectoryCollection -> URI -> DirectoryCollection
withURI (DirectoryCollection _ xs) u = DirectoryCollection u xs
withURI c _ = c

-- | Construct 'DirectoryCollection' from given 'FilePath'.
fromPath :: FilePath -> IO DirectoryCollection
fromPath p = let f = p <.> "yaml"
             in ifM (doesDirectoryExist p) (fromDirectory p) $
                    ifM (doesFileExist f) (fromFile f) $
                        return DoesNotExist

fromDirectory :: FilePath -> IO DirectoryCollection
fromDirectory d =
  do (es, xs) <- fmap partitionEithers . mapM (decodeFileEither . (d </>)) =<< listDirectory d
     
     if not (null es)
        then return . DoesNotParse . show . last $ es
        else return $ DirectoryCollection nullURI xs

fromFile :: FilePath -> IO DirectoryCollection
fromFile = fmap (either (DoesNotParse . show) $ DirectoryCollection nullURI . (:[])) . decodeFileEither
