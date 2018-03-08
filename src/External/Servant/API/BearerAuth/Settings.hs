{-|
Module      : External.Servant.API.BearerAuth.Settings
Description : Servant Bearer Authorization Setings
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Settings for "Servant" Bearer authorization.
-}
module External.Servant.API.BearerAuth.Settings where

import Crypto.JWT (JWTValidationSettings)
import Network.URI (URI)

data BearerSettings = BearerSettings
  { jwksURI               :: URI
  , jwtValidationSettings :: JWTValidationSettings
  , audience              :: URI
  }
