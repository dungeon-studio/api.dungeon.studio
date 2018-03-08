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
import Data.SirenJSON (Link (..))
import Data.Text ()
import Data.UUID (toString, UUID)
import Network.HTTP.Media ((//))
import Network.URI (URI)
import Servant ((:>), (:<|>) ((:<|>)), addHeader, AuthProtect, Capture, DeleteNoContent, err404, err500, FormUrlEncoded, Get, Handler, Header, Headers, JSON, NoContent (NoContent), PostCreated, ReqBody, Server, throwError)

import Characters.Types
import Errors
import External.Network.URI.HttpApiData ()
import External.Servant.API.BearerAuth (Claims (Claims, sub), hasScope)
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
characters uri s cs@Claims{sub = o} =
  do cs `hasScope` "read:characters"

     cs' <- Q.all (neo4j s) o `catch` throwServantErr
     return $ cs' uri ls
  where ls = [ Link { lClass = [ "Races" ], lRel = [ "races" ], lHref = races s, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Earthdawn Races" }
             , Link { lClass = [ "Disciplines" ], lRel = [ "disciplines" ], lHref = disciplines s, lType = Just $ "application" // "vnd.collection+json", lTitle = Just "Earthdawn Disciplines" }
             ]

create :: URI -> Settings -> Claims -> NewCharacter -> Handler (Headers '[Header "Location" URI] Character)
create uri s cs@Claims{sub = o} n =
  do cs `hasScope` "create:characters"

     c <- ($ uri) <$> Q.create (neo4j s) o n `catch` throwServantErr

     return $ addHeader (url c) c

character :: URI -> Settings -> Claims -> UUID -> Handler Character
character uri s cs@Claims{sub = o} u =
  do cs `hasScope` "read:characters"

     ($ uri) <$> Q.fromUUID (neo4j s) o u `catch` throwServantErr

delete :: Settings -> Claims -> UUID -> Handler NoContent
delete s cs@Claims{sub = o} u =
  do cs `hasScope` "delete:characters"

     Q.delete (neo4j s) o u `catch` throwServantErr

     return NoContent

throwServantErr :: Q.CharacterException -> Handler a
throwServantErr (Q.DoesNotExist u)     = throwError $ problem err404 $ toString u ++ " not found"
throwServantErr Q.DidNotCreate         = throwError $ problem err500 "could not create character" -- Shouldn't be thrown.
throwServantErr (Q.CharacterIOError e) = throwError $ problem err500 $ show e
throwServantErr (Q.PropertyError m)    = throwError $ problem err500 $ "property error: " ++ m
