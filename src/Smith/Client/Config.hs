{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Config (
    Smith (..)
  , SmithEndpoint (..)
  , configure
  , configureWith
  , smithScopes
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))

import           Crypto.JWT ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson ((.:))
import qualified Data.ByteString as ByteString
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.OAuth2.JWT.Client as OAuth2

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.FilePath ((</>))


newtype SmithEndpoint =
  SmithEndpoint {
      getSmithEndpoint :: Text
    } deriving (Eq, Ord, Show)

data SmithConfigureError =
    SmithConfigureJwkNotFoundError
  | SmithConfigureJwkNotFoundInSmithHomeError
  | SmithConfigureJsonDecodeError
  | SmithConfigureJwkDecodeError
  | SmithConfigureIdentityIdDecodeError Text
    deriving (Eq, Ord, Show)

data Smith =
    Smith SmithEndpoint HTTP.Manager OAuth2.Store

smithScopes :: [OAuth2.Scope]
smithScopes =
  [OAuth2.Scope "profile", OAuth2.Scope "ca"]

configure :: ExceptT SmithConfigureError IO Smith
configure = do
  liftIO (HTTP.newManager HTTP.tlsManagerSettings) >>=
    configureWith  smithScopes

configureWith :: [OAuth2.Scope] -> HTTP.Manager -> ExceptT SmithConfigureError IO Smith
configureWith scopes manager = do
  e <- liftIO $ Environment.lookupEnv "SMITH_ENDPOINT"
  j <- liftIO $ Environment.lookupEnv "SMITH_JWK"
  keydata <- case j of
    Nothing -> do
      s <- liftIO $ Environment.lookupEnv "SMITH_HOME"
      case s of
        Nothing -> do
          h <- liftIO Directory.getHomeDirectory
          ex <- liftIO . Directory.doesFileExist $ h </> ".smith" </> "credentials.json"
          case ex of
            False ->
              left SmithConfigureJwkNotFoundError
            True ->
              liftIO . ByteString.readFile $ h </> ".smith" </> "credentials.json"
        Just ss -> do
          ex <- liftIO $ Directory.doesFileExist (ss </> "credentials.json")
          case ex of
            False ->
              left SmithConfigureJwkNotFoundInSmithHomeError
            True ->
              liftIO . ByteString.readFile $ ss </> "credentials.json"
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

  issuer <- case Aeson.parse (Aeson.withObject "JWK" $ \o -> o .: "smith.st/identity-id") json of
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
        scopes
        (OAuth2.ExpiresIn 3600)
        []

  store <- lift $ OAuth2.newStore manager token claims jwk

  pure $ Smith endpoint manager store


left :: Monad m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
