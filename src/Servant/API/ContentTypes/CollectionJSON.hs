{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Servant.API.ContentTypes.Collection
Description : Servant ContentType for 'application/vnd.collection+json'
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Servant ContentType for 'application/vnd.collection+json'---a content type
created by M. Amundsen and documented at
http://amundsen.com/media-types/collection/.
-}
module Servant.API.ContentTypes.CollectionJSON where

import Control.Arrow (right)
import Data.Aeson (eitherDecode, encode)
import Network.HTTP.Media ((//))
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))

import Data.CollectionJSON

-- | An empty data type for Servant's content type logic.
data CollectionJSON

-- | @application/vnd.collection+json@
instance Accept CollectionJSON where
  contentType _ = "application" // "vnd.collection+json"

-- | 'MimeRender' instance for any instance of 'ToCollection'.
instance ToCollection a => MimeRender CollectionJSON a where
  mimeRender _ = encode . toCollection

-- | 'MimeUnrender' instance for any instance of 'FromCollection'.
instance FromCollection a => MimeUnrender CollectionJSON a where
  mimeUnrender _ = right fromCollection . eitherDecode
