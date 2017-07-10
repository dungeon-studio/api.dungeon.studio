{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.API.ContentTypes.Collection where

import Control.Arrow (right)
import Data.Aeson (eitherDecode, encode)
import Network.HTTP.Media ((//))
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))

import Data.Amundsen.Collection

data CollectionJSON

instance Accept CollectionJSON where
  contentType _ = "application" // "vnd.collection+json"

instance ToCollection a => MimeRender CollectionJSON a where
  mimeRender _ = encode . toCollection

instance FromCollection a => MimeUnrender CollectionJSON a where
  mimeUnrender _ = right fromCollection . eitherDecode
