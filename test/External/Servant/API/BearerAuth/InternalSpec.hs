{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : External.Servant.API.BearerAuth.InternalSpec
Description : Tests for External.Servant.API.BearerAuth.Internal
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Servant.API.BearerAuth.Internal".
-}
module External.Servant.API.BearerAuth.InternalSpec (main, spec) where

import Control.Lens ((.~), (&), (?~))
import Crypto.JWT (claimSub, emptyClaimsSet, unregisteredClaims)
import Data.Aeson (Value (String))
import Network.URI (nullURI)
import Test.Hspec (describe, hspec, it, pending, Selector, shouldReturn, shouldThrow, Spec)

import qualified Data.HashMap.Lazy as HashMap (fromList)

import External.Servant.API.BearerAuth.Internal
import External.Servant.API.BearerAuth.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "claims" $
    do it "should throw InvalidToken \"sub claim missing\"" $
         let cs = emptyClaimsSet
         in claims nullURI cs `shouldThrow` invalidToken "sub claim missing"

       it "should throw InvalidToken \"sub claim invalid\"" $
         let cs = emptyClaimsSet &
                    (claimSub ?~ "mailto://user@example.com")
         in claims nullURI cs `shouldThrow` invalidToken "sub claim invalid"

       it "should throw InvalidToken \"scope claim missing\"" $
         let cs = emptyClaimsSet &
                    (claimSub ?~ "user@example.com")
         in claims nullURI cs `shouldThrow` invalidToken "scope claim missing"

       it "should throw InvalidToken \"scope claim invalid\"" pending

       it "should return valid Claims" $
         let cs = emptyClaimsSet &
                    (claimSub ?~ "subject") &
                    unregisteredClaims .~ HashMap.fromList [ ("scope", String "s1 s2") ]
         in claims nullURI cs `shouldReturn` Claims { sub = "subject", scope = [ "s1", "s2" ], audience = nullURI } 

invalidToken :: String -> Selector BearerError
invalidToken s (InvalidToken s') = s == s'
invalidToken _ _                 = False
