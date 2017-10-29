{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Character.ListSpec
Description : Testing for Character Listings
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Testing for Character Listings.
-}
module Character.ListSpec (spec) where

import Network.HTTP.Simple (getResponseHeader, getResponseStatusCode, httpJSON, httpNoBody, setRequestHeader, setRequestHeaders, setRequestPath)
import Network.HTTP.Types (hAuthorization, hContentType)
import Test.Hspec (context, describe, hspec, it, shouldBe, Spec)

import qualified Data.ByteString as BS (append)
import qualified Data.ByteString.Char8 as BS (pack)

import Internal.Test.Contexts.Docker.Compose (withCompose)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  let p = "/characters"
  in around (withCompose "dungeon-studio") $
     describe ("GET " ++ p) $
       -- TODO Factor out HOST and PORT with setRequestHost and setRequestPort (docker context)
       do let s' = setRequestPath (BS.pack p) $
                   setRequestHeaders []
                   s

          context "with no Authorization" $ \ s ->
            do r <- httpNoBody s'

               it "should respond with 401" $
                  getResponseStatusCode r `shouldBe` 401

          context "with Authorization: Bearer " ++ token $
            do let s'' = setRequestHeader hAuthorization ["Bearer " `BS.append` token] s'

               r <- httpJSON s''
               
               it "should respond with 200" $
                  getResponseStatusCode r `shouldBe` 200

               it "should have header `Content-Type: application/vnd.siren+json`" $
                  getResponseHeader hContentType r `shouldBe` ["application/vnd.siren+json"]
