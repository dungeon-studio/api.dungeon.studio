{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Internal.JWT.Types
Description : JWT Types and Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Types and funcitons for JWT handling.
-}
module Internal.JWT.Types
  ( Claims
      ( Claims
      , sub
      , scope
      )
  , claims
  , OAuth2Error
      ( InvalidToken
      , InsufficientScope
      , MissingAuthorization
      )
  ) where

import Control.Lens (view)
import Crypto.JWT (ClaimsSet, claimSub, unregisteredClaims)
import Data.Aeson (fromJSON, Result (Error, Success))
import Data.Either.Utils (maybeToEither)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map (lookup)
import qualified Data.Text as T (pack, words)

-- | Subject and Scope from JWT.
--   TODO Consider other claims to be passed through.
--   TODO Consider a class as well, so others may define their own.
data Claims = Claims
  { sub   :: Text
  , scope :: [Text]
  }

claims :: ClaimsSet -> Either String Claims
claims c =
  do sub   <- maybeToEither "sub claim missing" $ T.pack . show <$> view claimSub c

     other <- maybeToEither "scope claim missing" $ Map.lookup "scope" $ view unregisteredClaims c
     scope <- case fromJSON other of
                Success x -> return $ T.words x
                Error m   -> fail $ "malformed scope: " ++ m

     return Claims {..}

data OAuth2Error = InvalidToken String
                 | InsufficientScope String
                 | MissingAuthorization
