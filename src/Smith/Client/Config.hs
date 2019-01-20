-- |
-- Smith client configuration data and functions.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Config (
    Smith (..)
  , SmithEndpoint (..)

  , smithScopes
  , configure
  , configureWith

  , SmithConfigureError (..)
  , renderSmithConfigureError
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))

import           Crypto.JWT (JWK)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson ((.:))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.OAuth2.JWT.Client as OAuth2

import           Smith.Client.Data.Identity (IdentityId (..))

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.FilePath (FilePath, (</>))


newtype SmithEndpoint =
  SmithEndpoint {
      getSmithEndpoint :: Text
    } deriving (Eq, Ord, Show)

data SmithCredentialsType =
    EnvironmentCredentials
  | SmithHomeCredentials FilePath
  | HomeCredentials FilePath
  | SuppliedCredentials
    deriving (Eq, Ord, Show)

data SmithCredentials =
    SmithCredentials SmithCredentialsType IdentityId JWK
    deriving (Eq, Ord, Show)

data SmithConfigureError =
    SmithConfigureCredentialsNotFound SmithCredentialsType
  | SmithConfigureJsonDecodeError SmithCredentialsType
  | SmithConfigureJwkDecodeError SmithCredentialsType
  | SmithConfigureIdentityIdDecodeError SmithCredentialsType
    deriving (Eq, Ord, Show)

renderSmithConfigureError :: SmithConfigureError -> Text
renderSmithConfigureError e =
  case e of
     SmithConfigureCredentialsNotFound t ->
       mconcat [
         "Smith JWK could not be located, generate a key and save it as '$HOME/.smith/credentials.json'."
       ]

    SmithConfigureJwkNotFoundInSmithHomeError ->
      "Smith JWK could not be located, you have $SMITH_HOME set, generate a key and save it as '$SMITH_HOME/credentials.json'."
    SmithConfigureJsonDecodeError ->
      "Smith credentials are not valid json, please verify could not be located, you have $SMITH_HOME set, generate a key and save it as '$SMITH_HOME/credentials.json'."
    SmithConfigureJwkDecodeError ->
      ""
    SmithConfigureIdentityIdDecodeError ->
      ""

data Smith =
    Smith SmithEndpoint HTTP.Manager OAuth2.Store


smithScopes :: [OAuth2.Scope]
smithScopes =
  [OAuth2.Scope "profile", OAuth2.Scope "ca"]


configure :: ExceptT SmithConfigureError IO Smith
configure = do
  liftIO (HTTP.newManager HTTP.tlsManagerSettings) >>=
    configureWith smithScopes


configureWith :: [OAuth2.Scope] -> HTTP.Manager -> ExceptT SmithConfigureError IO Smith
configureWith scopes manager = do
  e <- liftIO . maybe (SmithEndpoint "https://api.smith.st") (SmithEndpoint . Text.pack) $
    Environment.lookupEnv "SMITH_ENDPOINT"
  SmithCredentials t issuer jwk <- readCredentials
  let
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


readCredentials :: ExceptT SmithConfigureError IO SmithCredentials
readCredentials = do
  j <- liftIO $ Environment.lookupEnv "SMITH_JWK"
  keydata <- case j of
    Nothing -> do
      s <- liftIO $ Environment.lookupEnv "SMITH_HOME"
      case s of
        Nothing -> do
          h <- liftIO Directory.getHomeDirectory
          let path = h </> ".smith" </> "credentials.json"
          readCredentialsFile (SmithHomeCredentials path) path
        Just ss -> do
          let path = ss </> "credentials.json"
          readCredentialsFile (SmithHomeCredentials path) path
    Just s ->
      readCredentialsByteString EnvironmentCredentials . Text.encodeUtf8 . Text.pack $ s


readCredentialsByteString :: SmithCredentialsType -> ByteString -> ExceptT SmithConfigureError IO SmithCredentials
readCredentialsByteString t keydata =
  json <- case Aeson.decodeStrict keydata of
    Nothing ->
      left SmithConfigureJsonDecodeError t
    Just v ->
      pure v

  jwk <- case Aeson.decodeStrict keydata of
    Nothing ->
      left SmithConfigureJwkDecodeError t
    Just v ->
      pure v

  issuer <- case Aeson.parse (Aeson.withObject "JWK" $ \o -> o .: "smith.st/identity-id") json of
    Aeson.Error _msg ->
      left . SmithConfigureIdentityIdDecodeError t
    Aeson.Success v ->
      pure $ IdentityId . Text.pack . show $ (v :: Int64)

  pure $ SmithCredentials t issuer jwk


readCredentialsFile :: SmithCredentialsType -> FilePath -> ExceptT SmithConfigureError IO SmithCredentials
readCredentialsFile t path = do
  exists <- liftIO . Directory.doesFileExist $ h </> ".smith" </> "credentials.json"
  case exists of
    False ->
      left SmithConfigureCredentialsNotFoundError
    True -> do
      bytes <- liftIO . ByteString.readFile $ path
      readCredentialsByteString t bytes


left :: Monad m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
