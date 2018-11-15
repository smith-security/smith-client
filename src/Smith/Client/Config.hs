{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Config (
    Smith (..)
  , SmithEndpoint (..)
  , configure
  , configureWith
  ) where

import           Crypto.JWT ()

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.OAuth2.JWT.Client as OAuth2

import qualified System.Environment as Environment


newtype SmithEndpoint =
  SmithEndpoint {
      getSmithEndpoint :: Text
    } deriving (Eq, Ord, Show)

data Smith =
  Smith SmithEndpoint HTTP.Manager OAuth2.Store

configure :: IO Smith
configure = do
  HTTP.newManager HTTP.tlsManagerSettings >>= configureWith

configureWith :: HTTP.Manager -> IO Smith
configureWith manager = do
  e <- Environment.lookupEnv "SMITH_ENDPOINT"
  i <- Environment.lookupEnv "SMITH_ID"
  j <- Environment.lookupEnv "SMITH_JWK"
  let
    endpoint =
      case e of
        Nothing ->
          SmithEndpoint "https://api.smith.st"
        Just x ->
          SmithEndpoint . Text.pack $ x

    issuer =
      case i of
        Nothing ->
          ""
        Just x ->
          Text.pack x

    token =
      OAuth2.TokenEndpoint $
        mconcat [getSmithEndpoint endpoint, "/oauth/token"]

    claims =
      OAuth2.Claims
        (OAuth2.Issuer issuer)
        Nothing
        (OAuth2.Audience "https://smith.st")
        [OAuth2.Scope "profile", OAuth2.Scope "ca"] -- FIX
        (OAuth2.ExpiresIn 3600)
        []

    jwk =
      case j of
        Nothing ->
          error "todo"
        Just jj ->
          case Aeson.decodeStrict . Text.encodeUtf8 . Text.pack $ jj of
            Nothing ->
              error "todox"
            Just v ->
              v

  store <- OAuth2.newStore manager token claims jwk

  pure $ Smith endpoint manager store
