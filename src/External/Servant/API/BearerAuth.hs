{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  ( BearerSettings (..)
  , Claims
    ( Claims
    , sub
    , scope
    )
  , hasScope
  , handler
  ) where

import Control.Monad ((<=<), when)
import Control.Monad.Catch (handle, MonadThrow)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Crypto.JOSE (decodeCompact, JWKSet)
import Crypto.JWT (JWTError, verifyClaims)
import Data.Aeson (eitherDecode)
import Data.List (isInfixOf)
import Data.Text (Text)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Network.URI (URI, uriToString)
import Network.Wai (Request, requestHeaders)
import Servant (Handler, ServantErr)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Text.Regex (mkRegex, subRegex)

import qualified Data.ByteString as BS (stripPrefix)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import qualified Data.Text as T (unpack)

import External.Network.URI.HttpApiData ()
import External.Servant.API.BearerAuth.Errors
import External.Servant.API.BearerAuth.Internal
import External.Servant.API.BearerAuth.Settings
import External.Servant.API.BearerAuth.Types

hasScope :: Claims -> Text -> Handler ()
hasScope Claims{scope = ss, audience = a} s = when (s `notElem` ss) $ throwError $ e403 a $ InsufficientScope $ T.unpack s

handler :: BearerSettings -> AuthHandler Request Claims
handler BearerSettings{..} = mkAuthHandler $ \ r ->
  do k  <- jwks jwksURI
     c  <- maybe (throwError $ e401 audience MissingAuthorization) return $ compact r

     cs <- liftIO $ runExceptT $ decodeCompact c >>= verifyClaims jwtValidationSettings k

     either (throwError . jwtError) (handle (throwError . e401 audience) . claims audience) cs

  where jwtError = e401 audience . InvalidToken . show :: JWTError -> ServantErr

jwks :: (MonadThrow m, MonadIO m) => URI -> m JWKSet
jwks u =
  do r  <- parseRequest $ uriToString id u ""
     bs <- fix . getResponseBody <$> httpLBS r
     either fail return $ eitherDecode bs
  where fix = if "auth0" `isInfixOf` uriToString id u ""
                 then BL.pack . flip (subRegex re) "" . BL.unpack -- HACK for Auth0
                 else id
        re  = mkRegex "[,{][[:space:]]*\"x5t\":[^,}]+"

compact :: Request -> Maybe BL.ByteString
compact = fmap BL.fromStrict . (BS.stripPrefix "Bearer " <=< lookup "authorization") . requestHeaders
