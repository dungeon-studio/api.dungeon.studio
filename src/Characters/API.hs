{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Characters.API
Description : HTTP API for Characters
Copyright   : (c) Alex Brandt, 2017
License     : MIT

HTTP API for characters.
-}
module Characters.API
  ( API
  , server
  ) where

import Control.Monad.Catch (catch)
import Control.Monad (when)
import Data.UUID (toString, UUID)
import Network.URI (URI)
import Servant ((:>), (:<|>) ((:<|>)), addHeader, AuthProtect, Capture, DeleteNoContent, err403, err404, err500, FormUrlEncoded, Get, Handler, Header, Headers, JSON, NoContent (NoContent), PostCreated, ReqBody, Server, throwError)

import Characters.Types
import Errors
import External.Network.URI.HttpApiData ()
import External.Servant.API.BearerAuth (Claims (Claims, scope, sub))
import External.Servant.API.ContentTypes.SirenJSON (SirenJSON)
import Settings

import qualified Characters.Queries as Q

-- | "Servant" API for Characters.
type API = AuthProtect "jwt" :> Get '[SirenJSON] Characters
      :<|> AuthProtect "jwt" :> ReqBody '[FormUrlEncoded, JSON] NewCharacter :> PostCreated '[SirenJSON] (Headers '[Header "Location" URI] Character)
      :<|> AuthProtect "jwt" :> Capture "character-uuid" UUID :> Get '[SirenJSON] Character
      :<|> AuthProtect "jwt" :> Capture "character-uuid" UUID :> DeleteNoContent '[SirenJSON] NoContent

-- | "Servant" 'Server' for Characters.
server :: URI -> Settings -> Server API
server uri s = characters uri s
          :<|> create uri s
          :<|> character uri s
          :<|> delete s

characters :: URI -> Settings -> Claims -> Handler Characters
characters uri s Claims{sub = o, scope = ss} = 
  do when ("read:characters" `notElem` ss) $ throwError err403 -- TODO use custom error definition

     ($ uri) <$> Q.all (neo4j s) o `catch` throwServantErr

create :: URI -> Settings -> Claims -> NewCharacter -> Handler (Headers '[Header "Location" URI] Character)
create uri s Claims{sub = o, scope = ss} n =
  do when ("create:characters" `notElem` ss) $ throwError err403 -- TODO use custom error definition

     c <- ($ uri) <$> Q.create (neo4j s) o n `catch` throwServantErr

     return $ addHeader (url c) c

character :: URI -> Settings -> Claims -> UUID -> Handler Character
character uri s Claims{sub = o, scope = ss} u =
  do when ("read:characters" `notElem` ss) $ throwError err403 -- TODO use custom error definition
     
     ($ uri) <$> Q.fromUUID (neo4j s) o u `catch` throwServantErr

delete :: Settings -> Claims -> UUID -> Handler NoContent
delete s Claims{sub = o, scope = ss} u =
  do when ("delete:characters" `notElem` ss) $ throwError err403 -- TODO use custom error definition

     Q.delete (neo4j s) o u `catch` throwServantErr

     return NoContent

throwServantErr :: Q.CharacterException -> Handler a
throwServantErr (Q.DoesNotExist u)     = throwError $ problem err404 $ toString u ++ " not found"
throwServantErr Q.DidNotCreate         = throwError $ problem err500 "could not create character" -- Shouldn't be thrown.
throwServantErr (Q.CharacterIOError e) = throwError $ problem err500 $ show e
throwServantErr (Q.PropertyError m)    = throwError $ problem err500 $ "property error: " ++ m
