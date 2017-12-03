{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : External.Data.Problem
Description : Types and Instances for @application/problem+json@
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and instances for @application/problem+json@.

Full documentation for @application/problem+json@ can be found at
<https://tools.ietf.org/html/rfc7807>.
-}
module External.Data.Problem where

import Data.Aeson ((.=), (.:?), (.!=), FromJSON (parseJSON), object, ToJSON (toJSON), withObject)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Network.URI.JSON ()
import Network.URI (parseURI, URI)

data Problem = Problem
  { pType     :: Maybe URI  -- ^ when dereferenced, should provide human-readable
                            --   documenation for the problem type
  , pTitle    :: Maybe Text -- ^ human-readable summary; should not change
                            --   between occurrences
  , pStatus   :: Maybe Int  -- ^ match the HTTP status code on the error
  , pDetail   :: Maybe Text -- ^ human-readable explanation
  , pInstance :: Maybe URI  -- ^ when dereferenced, may or may not yield further
                            --   information about the particular problem
                            --   instance
  -- TODO Add extensions
  } deriving (Eq, Show)

instance FromJSON Problem where
  parseJSON = withObject "Problem" $ \ v ->
    do pType     <- v .:? "type"     .!= parseURI "about:blank"
       pTitle    <- v .:? "title"
       pStatus   <- v .:? "status"
       pDetail   <- v .:? "detail"
       pInstance <- v .:? "instance"

       return Problem{..}

instance ToJSON Problem where
  toJSON Problem{..} = object $ catMaybes
    [ (.=) "type"     <$> pType
    , (.=) "title"    <$> pTitle
    , (.=) "status"   <$> pStatus
    , (.=) "detail"   <$> pDetail
    , (.=) "instance" <$> pInstance
    ]
