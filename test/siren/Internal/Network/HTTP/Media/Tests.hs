{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Internal.Network.URI.Tests
Description : MediaType Arbitrary Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of 'Arbitrary' instances for 'MediaType'.
-}
module Internal.Network.HTTP.Media.Tests where

import Data.ByteString (ByteString)
import Network.HTTP.Media ((//), (/:), MediaType)
import Test.QuickCheck (Arbitrary (arbitrary), elements, listOf, suchThat)
import Test.QuickCheck.Instances ()

instance Arbitrary MediaType where
  arbitrary = 
    do m   <- (//) <$> elements mts <*> (arbitrary `suchThat` ((`elem` (['A'..'Z'] ++ ['a'..'z'])) . head . show))

       ps  <- listOf $ (,) <$> arbitrary <*> arbitrary

       return $ foldl (/:) m ps
    where mts :: [ByteString]
          mts = [ "application"
                , "audio"
                , "image"
                , "message"
                , "model"
                , "multipart"
                , "text"
                , "video"
                ]
