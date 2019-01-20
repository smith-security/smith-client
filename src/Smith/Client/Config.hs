{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Config (
    Smith (..)
  , SmithEndpoint (..)
  , configure
  , configureWith
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))

import           Crypto.JWT ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson ((.:))
import           Data.Int (Int64)
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

data SmithConfigureError =
    SmithConfigureJwkNotFoundError
  | SmithConfigureJsonDecodeError
  | SmithConfigureJwkDecodeError
  | SmithConfigureIdentityIdDecodeError Text
    deriving (Eq, Ord, Show)

data Smith =
    Smith SmithEndpoint HTTP.Manager OAuth2.Store

configure :: ExceptT SmithConfigureError IO Smith
configure = do
  liftIO (HTTP.newManager HTTP.tlsManagerSettings) >>=
    configureWith

configureWith :: HTTP.Manager -> ExceptT SmithConfigureError IO Smith
configureWith manager = do
  e <- liftIO $ Environment.lookupEnv "SMITH_ENDPOINT"
  j <- liftIO $ Environment.lookupEnv "SMITH_JWK"
  keydata <- case j of
    Nothing ->
      left SmithConfigureJwkNotFoundError
    Just s ->
      pure . Text.encodeUtf8 . Text.pack $ s

  json <- case Aeson.decodeStrict keydata of
    Nothing ->
      left SmithConfigureJsonDecodeError
    Just v ->
      pure v

  jwk <- case Aeson.decodeStrict keydata of
    Nothing ->
      left SmithConfigureJwkDecodeError
    Just v ->
      pure v

  let
    extractor =
      Aeson.withObject "JWK" $ \o -> o .: "smith.st/identity-id"

  issuer <- case Aeson.parse extractor json of
    Aeson.Error msg ->
      left . SmithConfigureIdentityIdDecodeError . Text.pack $ msg
    Aeson.Success v ->
      pure $ Text.pack . show $ (v :: Int64)

  let
    endpoint =
      case e of
        Nothing ->
          SmithEndpoint "https://api.smith.st"
        Just x ->
          SmithEndpoint . Text.pack $ x

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

  store <- lift $ OAuth2.newStore manager token claims jwk

  pure $ Smith endpoint manager store


left :: Monad m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
