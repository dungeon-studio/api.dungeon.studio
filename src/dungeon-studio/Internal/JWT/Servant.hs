{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Internal.JWT.Servant
Description : JWT Servant Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Servant" types for JWT authentication.
-}
module Internal.JWT.Servant
  ( handler
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Control.Monad ((<=<), when)
import Crypto.JOSE (decodeCompact, JWKSet)
import Crypto.JWT (JWTError, JWTValidationSettings, verifyClaims)
import Data.Either.Combinators (whenLeft)
import Data.Maybe (fromJust, isNothing)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import Network.URI (URI)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, err401, err500)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import qualified Data.ByteString as BS (stripPrefix)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict)

import Internal.JWT.Types
import Internal.Network.URI ()

type instance AuthServerData (AuthProtect "jwt") = Claims

handler :: URI -> JWTValidationSettings -> AuthHandler Request Claims
handler u v = mkAuthHandler $ \ r -> do
    k <- liftIO $ runExceptT $ jwks u
    whenLeft k $ const $ throwError err500 -- TODO Error Content
    let Right k' = k

    let compact' = compact r
    when (isNothing compact') $ throwError err401 -- TODO Error Content
    
    result <- liftIO $ runExceptT $ decodeCompact (fromJust compact') >>= verifyClaims v k'
    whenLeft result $ \ (_ :: JWTError) -> throwError err401 -- TODO Error Content
    let Right result' = result

    either (const $ throwError err401) return $ claims result' -- TODO Error Content

compact :: Request -> Maybe BL.ByteString
compact = fmap BL.fromStrict . (BS.stripPrefix "Bearer " <=< lookup "authorization") . requestHeaders

jwks :: (MonadThrow m, MonadIO m) => URI -> m JWKSet
jwks = return . getResponseBody <=< httpJSON <=< parseRequest . show
