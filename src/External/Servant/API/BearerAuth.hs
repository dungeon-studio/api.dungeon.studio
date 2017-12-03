{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : External.Servant.API.BearerAuth
Description : Servant Bearer Authentication
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Servant" types for Bearer authentication.
-}
module External.Servant.API.BearerAuth
  ( Claims
      ( Claims
      , sub
      , scope
      )
  , handler
  ) where

import Control.Monad.Catch (handle)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans (liftIO)
import Crypto.JOSE (decodeCompact)
import Crypto.JWT (JWTError, JWTValidationSettings, verifyClaims)
import Network.URI (URI)
import Network.Wai (Request)
import Servant (AuthProtect, ServantErr)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import External.Network.URI.HttpApiData ()
import External.Servant.API.BearerAuth.Internal

type instance AuthServerData (AuthProtect "jwt") = Claims

handler :: URI -> JWTValidationSettings -> AuthHandler Request Claims
handler u v = mkAuthHandler $ \ r ->
  do k  <- jwks u
     c  <- maybe (throwError $ e401 MissingAuthorization) return $ compact r

     cs <- liftIO $ runExceptT $ decodeCompact c >>= verifyClaims v k

     either (throwError . jwtError) (handle (throwError . e401) . claims) cs

  where jwtError = e401 . InvalidToken . show :: JWTError -> ServantErr
