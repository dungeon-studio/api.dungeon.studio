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
  ( Claims
      ( Claims
      , sub
      , scope
      )
  , handler
  , publicJWKs
  ) where

import Control.Lens (view)
import Control.Monad.Catch (catch, MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad ((<=<), forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Crypto.JOSE (decodeCompact, JWK, JWKSet (JWKSet), JWKStore)
import Crypto.JWT (ClaimsSet, claimSub, JWTError, JWTValidationSettings, verifyClaims, unregisteredClaims)
import Data.Aeson (fromJSON, Result (Error, Success), Value (String))
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Text (Text)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import Network.URI (URI)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, err401, Handler, ServantErr)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import System.Envy (env, FromEnv (fromEnv))

import qualified Data.ByteString as BS (ByteString, stripPrefix)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict)
import qualified Data.HashMap.Strict as Map (lookup)
import qualified Data.Text as T (pack, words)

import Internal.JWT.Types
import Internal.Network.URI ()

type instance AuthServerData (AuthProtect "auth0") = Claims

handler :: URI -> JWTValidationSettings -> AuthHandler Request Claims
handler u v = mkAuthHandler $ \ r -> do
    j <- maybe (throwError err401) decodeCompact $ compact r
    k <- liftIO $ jwks u `catch` e500
    c <- liftIO $ verifyClaims v k j `catchError` e401

    either (throwError err401) return $ claims c

e500 :: (Exception e, MonadThrow m) => String -> e -> m ServantErr
e500 p e = throwError $ err500 { errBody = p ++ show e }

compact :: Request -> Maybe BL.ByteString
compact = BL.fromStrict <$> (BS.stripPrefix "Bearer" <=< lookup "authorization") . requestHeaders

jwks :: (MonadThrow m, MonadIO m, JWKStore a) => URI -> m a
jwks = fmap getResponseBody (httpJSON =<< parseRequest . show)
