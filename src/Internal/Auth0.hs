{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Internal.Auth0
Description : Auth0 Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Servant" types for Auth0 JWT authentication.
-}
module Internal.Auth0
  ( Claims
      ( Claims
      , sub
      , scope
      )
  , Environment
      ( audience
      , issuer
      , jwksURI
      )
  , handler
  , publicJWKs
  ) where

import Control.Lens (view)
import Control.Monad ((<=<), forM_, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans (liftIO)
import Crypto.JOSE (decodeCompact, JWK, JWKSet (JWKSet))
import Crypto.JWT (ClaimsSet, claimSub, jwtClaimsSet, JWTError, JWTValidationSettings, validateJWSJWT, unregisteredClaims)
import Data.Aeson (fromJSON, Result (Error, Success), Value (String))
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Text (Text)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import Network.URI (URI)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, err401, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import System.Envy (env, FromEnv (fromEnv))

import qualified Data.ByteString as BS (ByteString, stripPrefix)
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.HashMap.Strict as Map (lookup)
import qualified Data.Text as T (pack, words)

import Internal.Network.URI ()

-- | Subject and Scope from Auth0 JWT.
--   TODO Consider other claims to be passed through.
data Claims = Claims
  { sub   :: Text
  , scope :: [Text]
  }

type instance AuthServerData (AuthProtect "auth0") = Claims

data Environment = Environment
  { audience :: URI -- ^ Accepted OAuth2 Audience
  , issuer   :: URI -- ^ Accepted OAuth2 Issuer
  , jwksURI  :: URI -- ^ Authorization Server JWKS URL
  }

instance Show Environment where
  show Environment{..} =
    "AUTH0_AUDIENCE=" ++ show audience ++ "\n" ++
    "AUTH0_ISSUER=" ++ show issuer ++ "\n" ++
    "AUTH0_JWKS_URI" ++ show jwksURI ++ "\n"

instance FromEnv Environment where
  fromEnv = Environment
    <$> env "AUTH0_AUDIENCE"
    <*> env "AUTH0_ISSUER"
    <*> env "AUTH0_JWKS_URI"

-- | TODO This and 'p' should be merged into a single function or otherwise
--   refactored for readability.
handler :: URI -> JWTValidationSettings -> AuthHandler Request Claims
handler u s = mkAuthHandler $ claims u s . credentials

credentials :: Request -> BS.ByteString
credentials = fromMaybe mempty . (BS.stripPrefix "Bearer " <=< lookup "authorization" . requestHeaders)

claims :: URI -> JWTValidationSettings -> BS.ByteString -> Handler Claims
claims u s cs =
  do jwks  <- liftIO $ publicJWKs u

     joseClaimsSet <- liftIO . runExceptT $ do
                        jwt     <- decodeCompact . BL.fromStrict $ cs

                        forM_ jwks (\ k -> validateJWSJWT s k jwt)

                        return $ jwtClaimsSet jwt

     p joseClaimsSet -- TODO better name

publicJWKs :: URI -> IO [JWK]
publicJWKs u =
  do request  <- parseRequest $ show u
     response <- httpJSON request
     let (JWKSet jwks) = getResponseBody response -- TODO merge this and previous line?
     return jwks

p :: Either JWTError ClaimsSet -> Handler Claims
p (Left _)   = throwError err401 -- TODO Headers
p (Right cs) =
  do let sub = T.pack . show <$> view claimSub cs
     when (isNothing sub) $ throwError err401 -- TODO Add headers

     scope <- case fromJSON . fromMaybe (String mempty) . Map.lookup "scope" $ view unregisteredClaims cs of
                Success x -> return $ T.words x
                Error _   -> throwError err401 -- TODO Add headers

     return $ Claims (fromJust sub) scope
