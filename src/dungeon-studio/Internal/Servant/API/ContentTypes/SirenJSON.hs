{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Internal.Servant.API.ContentTypes.Siren
Description : Servant ContentType for @application/vnd.siren+json@
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Servant ContentType for @application/vnd.siren+json@---a content type
created by Kevin Swiber and documented at
<https://github.com/kevinswiber/siren>
-}
module Internal.Servant.API.ContentTypes.SirenJSON where

import Control.Arrow (right)
import Data.Aeson (eitherDecode, encode)
import Network.HTTP.Media ((//))
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))

import Internal.Data.SirenJSON (FromEntity (fromEntity), ToEntity (toEntity))

-- | Content type suitable for use with "Servant"
data SirenJSON

-- | @application/vnd.siren+json@
instance Accept SirenJSON where
  contentType _ = "applicatin" // "vnd.siren+json"

-- | Convert a 'ToEntity' instance to a 'ByteString'
instance ToEntity a => MimeRender SirenJSON a where
  mimeRender _ = encode . toEntity

-- | Convert a 'ByteString' to a 'FromEntity' instance
instance FromEntity a => MimeUnrender SirenJSON a where
  mimeUnrender _ = right fromEntity . eitherDecode
