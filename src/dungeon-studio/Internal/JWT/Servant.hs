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
import Network.HTTP.Types.Header (hWWWAuthenticate)
import Network.URI (URI)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, err401, err500, ServantErr (errHeaders))
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import qualified Data.ByteString as BS (stripPrefix)
import qualified Data.ByteString.Char8 as BS (pack)
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
    when (isNothing compact') $ throwError (e401 MissingAuthorization)
    
    result <- liftIO $ runExceptT $ decodeCompact (fromJust compact') >>= verifyClaims v k'
    whenLeft result $ \ (e :: JWTError) -> throwError (e401 $ InvalidToken $ show e)
    let Right result' = result

    either (\ e -> throwError (e401 $ InvalidToken e)) return $ claims result'

compact :: Request -> Maybe BL.ByteString
compact = fmap BL.fromStrict . (BS.stripPrefix "Bearer " <=< lookup "authorization") . requestHeaders

jwks :: (MonadThrow m, MonadIO m) => URI -> m JWKSet
jwks = return . getResponseBody <=< httpJSON <=< parseRequest . show

e401 :: OAuth2Error -> ServantErr -- TODO Make this suck less
e401 e = err401
           { errHeaders = [ (hWWWAuthenticate, BS.pack (h e)) ]
           }
  where h MissingAuthorization  = p
        h (InvalidToken d)      = p ++ ", error=\"invalid_token\", error_description=\"" ++ d ++ "\""
        h (InsufficientScope d) = p ++ ", error=\"insufficient_scope\", error_description=\"" ++ d ++ "\""
        
        p = "Bearer realm=\"AUDIENCE\"" :: String
