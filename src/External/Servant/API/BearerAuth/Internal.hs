{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : External.Servant.API.BearerAuth.Internal
Description : Servant Bearer Authentication Internals
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Servant" types for Bearer authentication.
-}
module External.Servant.API.BearerAuth.Internal where

import Control.Lens ((^?), (^.))
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad ((<=<))
import Crypto.JOSE (JWKSet)
import Crypto.JWT (ClaimsSet, claimSub, string, unregisteredClaims)
import Data.Aeson (eitherDecode, fromJSON, Result (Error, Success))
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Network.HTTP.Types.Header (hWWWAuthenticate)
import Network.URI (URI, uriToString)
import Network.Wai (Request, requestHeaders)
import Servant (err401, ServantErr (errHeaders))
import Text.Regex (mkRegex, subRegex)

import qualified Data.ByteString as BS (stripPrefix)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import qualified Data.HashMap.Strict as Map (lookup)
import qualified Data.Text as T (pack, words)

import External.Network.URI.HttpApiData ()

-- | Subject and Scope from JWT.
--
--   TODO Consider other claims to be passed through.
--   TODO Consider a class as well, so others may define their own.
data Claims = Claims
  { sub   :: Text
  , scope :: [Text]
  }
  deriving (Eq, Show)

data BearerError = InvalidToken String
                 | InsufficientScope String
                 | MissingAuthorization
  deriving (Typeable)

instance Show BearerError where
  show (InvalidToken d)      = "invalid_token: " ++ d
  show (InsufficientScope d) = "insufficient_scope: " ++ d
  show MissingAuthorization  = "missing authorization"

instance Exception BearerError

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

claims :: (MonadThrow m) => ClaimsSet -> m Claims
claims c =
  do sub'  <- maybe (throwM $ InvalidToken "sub claim missing") return (c ^. claimSub)
     sub   <- maybe (throwM $ InvalidToken "sub claim invalid") (return . T.pack) (sub' ^? string)

     other <- maybe (throwM $ InvalidToken "scope claim missing") return (Map.lookup "scope" $ c ^. unregisteredClaims)
     scope <- case fromJSON other of
                Success x -> return $ T.words x
                Error m   -> throwM $ InvalidToken $ "scope claim invalid: " ++ m

     return Claims {..}

e401 :: BearerError -> ServantErr
e401 e = err401 { errHeaders = [ (hWWWAuthenticate, BS.pack (h e)) ] }
  where h MissingAuthorization  = p
        h (InvalidToken d)      = p ++ ", error=\"invalid_token\", error_description=\"" ++ d ++ "\""
        h (InsufficientScope d) = p ++ ", error=\"insufficient_scope\", error_description=\"" ++ d ++ "\""

        p = "Bearer realm=\"AUDIENCE\"" :: String -- TODO Fill in Audience
