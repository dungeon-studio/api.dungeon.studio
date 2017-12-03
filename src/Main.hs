{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Main
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for api.dungeon.studio.
-}
module Main (main) where

import Control.Lens ((.~), (&), preview)
import Crypto.JWT (defaultJWTValidationSettings, issuerPredicate, stringOrUri)
import Data.Maybe (fromJust)
import Network.URI (nullURI)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai (Request)
import Servant (Application, Context ((:.), EmptyContext), Proxy (Proxy), serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler)
import System.Envy (decodeEnv)

import API
import Environment
import Initialize
import Settings
import External.Servant.API.BearerAuth

import qualified Internal.JWT.Environment as JWT (audience, issuer, jwksURI)

-- | Sets up and runs a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main = withStdoutLogger $ \ l ->
  do e <- either fail return =<< decodeEnv
     print e

     s <- settings e
     initialize s

     let w = setPort (port e) $
             setLogger l
             defaultSettings

     runSettings w $ application e s

application :: Environment -> Settings -> Application
application e = serveWithContext (Proxy :: Proxy API) (context e) . server nullURI -- TODO use a configured URI

context :: Environment -> Context (AuthHandler Request Claims ': '[])
context Environment { jwtEnvironment = s } = handler u v :. EmptyContext
  where u = JWT.jwksURI s
        v = defaultJWTValidationSettings (== (fromJust . preview stringOrUri . show $ JWT.audience s))
              & issuerPredicate .~ (== (fromJust . preview stringOrUri . show $ JWT.issuer s))
