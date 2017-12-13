{-|
Module      : External.Servant.API.BearerAuth.Errors
Description : Servant Bearer Authentication Errors
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"ServantErr" constructors for Bearer authorization.
-}
module External.Servant.API.BearerAuth.Errors where

import Network.HTTP.Types.Header (hWWWAuthenticate)
import Network.URI (URI)
import Servant (err401, err403, ServantErr (errHeaders))

import qualified Data.ByteString.Char8 as BS (pack)

import External.Servant.API.BearerAuth.Types

e401 :: URI -> BearerError -> ServantErr
e401 = err err401

e403 :: URI -> BearerError -> ServantErr
e403 = err err403

err :: ServantErr -> URI -> BearerError -> ServantErr
err s a e = s { errHeaders = [ (hWWWAuthenticate, BS.pack (h e)) ] }
  where h MissingAuthorization  = p
        h (InvalidToken d)      = p ++ ", error=\"invalid_token\", error_description=\"" ++ d ++ "\""
        h (InsufficientScope d) = p ++ ", error=\"insufficient_scope\", error_description=\"" ++ d ++ "\""

        p = "Bearer realm=\"" ++ show a ++ "\"" :: String
