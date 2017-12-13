{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : External.Servant.API.BearerAuth.Types
Description : Servant Bearer Authorization Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Servant" types for Bearer authorization.
-}
module External.Servant.API.BearerAuth.Types where

import Control.Monad.Catch (Exception)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.URI (URI)
import Servant (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)

type instance AuthServerData (AuthProtect "jwt") = Claims

-- | Subject and Scope from JWT.
--
--   TODO Consider other claims to be passed through.
--   TODO Consider a class as well, so others may define their own.
data Claims = Claims
  { sub      :: Text
  , scope    :: [Text]
  , audience :: URI
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
