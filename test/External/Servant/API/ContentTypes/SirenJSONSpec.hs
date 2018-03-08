{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Servant.API.ContentTypes.SirenJSONSpec
Description : Tests for External.Servant.API.ContentTypes.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Servant.API.ContentTypes.SirenJSON".
-}
module External.Servant.API.ContentTypes.SirenJSONSpec (main, spec) where

import Data.SirenJSON (Entity, FromEntity (fromEntity), ToEntity (toEntity))
import Servant.API (mimeRender, mimeUnrender)
import Servant (Proxy (Proxy))
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.Invariant ((<=>))

import External.Data.SirenJSON.Arbitrary ()
import External.Data.SirenJSON.Norm (normalize)
import External.Servant.API.ContentTypes.SirenJSON

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $ modifyMaxSize (const 25) $
    prop "mimeUnrender (Proxy :: Proxy SirenJSON) . mimeRender (Proxy :: Proxy SirenJSON) == Right . normalize"
      (mimeUnrender (Proxy :: Proxy SirenJSON) . mimeRender (Proxy :: Proxy SirenJSON) <=> Right . normalize :: Entity -> Bool)

instance FromEntity Entity where
  fromEntity = id

instance ToEntity Entity where
  toEntity = id
