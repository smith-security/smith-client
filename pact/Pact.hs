{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact (
    Provider (..)
  , Consumer (..)
  , Request (..)
  , Response (..)
  , Interaction (..)
  , Pact (..)
  , serialise
  ) where

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import           Data.Monoid ((<>))
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as  HTTP

newtype Provider =
  Provider {
      providerName :: Text
    } deriving (Eq, Ord, Show, IsString)

newtype Consumer =
  Consumer {
      consumerName :: Text
    } deriving (Eq, Ord, Show, IsString)

data Request =
  Request {
      requestMethod :: HTTP.StdMethod
    , requestPath :: Text
    , requestBody :: Maybe Aeson.Value
    } deriving (Eq, Show)

data Response =
  Response {
      responseStatus :: HTTP.Status
    , responseHeaders :: [HTTP.Header]
    , responseBody :: Maybe Aeson.Value
    } deriving (Eq, Show)

data Interaction =
  Interaction {
      interactionDescription :: Text
    , interactionRequest :: Request
    , interactionRespone :: Response
    } deriving (Eq, Show)

data Pact =
  Pact {
      pactProvider :: Provider
    , pactConsumer :: Consumer
    , pactInteractions :: [Interaction]
    } deriving (Eq, Show)

instance Aeson.ToJSON Interaction where
  toJSON (Interaction description request response) =
    Aeson.object [
        "description" .= description
      , "request" .= Aeson.object ([
            "method" .= (Text.decodeUtf8  . HTTP.renderStdMethod  . requestMethod) request
          , "path" .= requestPath request
          ] <> maybe [] (\v -> ["body" .= v]) (requestBody request))
      , "response" .= Aeson.object ([
            "status" .= HTTP.statusCode (responseStatus response)
          , "headers" .= Aeson.object (flip fmap (responseHeaders response) $ \(k, v) ->
              (Text.decodeUtf8 . CaseInsensitive.original) k .= Text.decodeUtf8 v)
          ] <> maybe [] (\v -> ["body" .= v]) (responseBody response))
      ]

instance Aeson.ToJSON Pact where
  toJSON (Pact provider consumer interactions) =
    Aeson.object [
        "provider" .= Aeson.object [
            "name" .= providerName provider
          ]
      , "consumer" .= Aeson.object [
            "name" .= consumerName consumer
          ]
      , "interactions" .= interactions
      , "metadata" .= Aeson.object [
            "pactSpecification" .= Aeson.object [
                  "version" .= ("3.0.0" :: Text)
                ]
           ]
      ]

serialise :: Pact -> LazyByteString.ByteString
serialise pact =
  Aeson.encode pact
