{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : External.Servant.API.BearerAuth.Internal
Description : Servant Bearer Authorization
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Servant" types for Bearer authorization.
-}
module External.Servant.API.BearerAuth.Internal where

import Control.Lens ((^?), (^.))
import Control.Monad.Catch (MonadThrow, throwM)
import Crypto.JWT (ClaimsSet, claimSub, string, unregisteredClaims)
import Data.Aeson (fromJSON, Result (Error, Success))
import Network.URI (URI)

import qualified Data.HashMap.Strict as Map (lookup)
import qualified Data.Text as T (pack, words)

import External.Servant.API.BearerAuth.Types

claims :: (MonadThrow m) => URI -> ClaimsSet -> m Claims
claims a c =
  do sub'  <- maybe (throwM $ InvalidToken "sub claim missing") return (c ^. claimSub)
     sub   <- maybe (throwM $ InvalidToken "sub claim invalid") (return . T.pack) (sub' ^? string)

     other <- maybe (throwM $ InvalidToken "scope claim missing") return (Map.lookup "scope" $ c ^. unregisteredClaims)
     scope <- case fromJSON other of
                Success x -> return $ T.words x
                Error m   -> throwM $ InvalidToken $ "scope claim invalid: " ++ m

     let audience = a

     return Claims {..}
