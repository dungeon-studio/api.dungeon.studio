{-|
Module      : Internal.JWT.Environment
Description : JWT Environment Variables
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Environment types for JWT authorization.
-}
module Internal.JWT.Environment where

data Environment = Environment
  { audience :: URI -- ^ Accepted Oauth2 Audience
  , issuer   :: URI -- ^ Accepted Oauth2 Issuer
  , jwksURI  :: URI -- ^ Authorization Server JWKS URL
  }

instance Show Environment where
  show Environment{..} =
    "JWT_AUDIENCE=" ++ show audience ++ "\n" ++
    "JWT_ISSUER=" ++ show issuer ++ "\n" ++
    "JWT_JWKS_URI" ++ show jwksURI ++ "\n"

instance FromEnv Environment where
  fromEnv = Environment
    <$> env "JWT_AUDIENCE"
    <*> env "JWT_ISSUER"
    <*> env "JWT_JWKS_URI"
