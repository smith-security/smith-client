-- |
-- Smith client configuration data and functions.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Config (
  -- * Smith client runtime data.
    Smith (..)

  -- * Smith client input configuration.
  , SmithEndpoint (..)
  , SmithCredentialsType (..)
  , SmithCredentials (..)

  -- * OAuth2 scopes
  , SmithScope (..)

  -- * High-level configuration operations.
  , configure
  , configureWith
  , configureT
  , configureWithT

  -- * Low-level configuration operations.
  , configureEndpoint
  , configureOAuth2
  , configureCredentials
  , configureCredentialsByteString
  , configureCredentialsFile

  -- * Configuration errors.
  , SmithConfigureError (..)
  , renderSmithConfigureError
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

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


data Smith =
    Smith SmithEndpoint HTTP.Manager OAuth2.Store


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
    deriving (Eq, Show)


data SmithScope =
    ProfileScope
  | CAScope
    deriving (Eq, Ord, Show, Enum, Bounded)


toOAuth2 :: SmithScope -> OAuth2.Scope
toOAuth2 s =
  case s of
    ProfileScope ->
      OAuth2.Scope "profile"
    CAScope ->
      OAuth2.Scope "ca"


configure :: IO (Either SmithConfigureError Smith)
configure =
  runExceptT configureT


configureT :: ExceptT SmithConfigureError IO Smith
configureT = do
  liftIO (HTTP.newManager HTTP.tlsManagerSettings) >>=
    configureWithT [minBound .. maxBound]


configureWith :: [SmithScope] -> HTTP.Manager -> IO (Either SmithConfigureError Smith)
configureWith scopes manager =
  runExceptT $ configureWithT scopes manager


configureWithT :: [SmithScope] -> HTTP.Manager -> ExceptT SmithConfigureError IO Smith
configureWithT scopes manager = do
  endpoint <- liftIO $ configureEndpoint
  SmithCredentials _ issuer jwk <- configureCredentials
  oauth2 <- liftIO $ configureOAuth2 manager endpoint (toOAuth2 <$> scopes) jwk issuer
  pure $ Smith endpoint manager oauth2


configureEndpoint :: IO SmithEndpoint
configureEndpoint =
  maybe (SmithEndpoint "https://api.smith.st") (SmithEndpoint . Text.pack) <$>
    Environment.lookupEnv "SMITH_ENDPOINT"


configureOAuth2 :: HTTP.Manager -> SmithEndpoint -> [OAuth2.Scope] -> JWK -> IdentityId -> IO OAuth2.Store
configureOAuth2 manager endpoint scopes jwk issuer =
  let
    token =
      OAuth2.TokenEndpoint $
        mconcat [getSmithEndpoint endpoint, "/oauth/token"]

    claims =
      OAuth2.Claims
        (OAuth2.Issuer $ identityId issuer)
        Nothing
        (OAuth2.Audience "https://smith.st")
        scopes
        (OAuth2.ExpiresIn 3600)
        []
   in
     OAuth2.newStore manager token claims jwk


configureCredentials :: ExceptT SmithConfigureError IO SmithCredentials
configureCredentials = do
  j <- liftIO $ Environment.lookupEnv "SMITH_JWK"
  case j of
    Nothing -> do
      s <- liftIO $ Environment.lookupEnv "SMITH_HOME"
      case s of
        Nothing -> do
          h <- liftIO Directory.getHomeDirectory
          let path = h </> ".smith" </> "credentials.json"
          configureCredentialsFile (SmithHomeCredentials path) path
        Just ss -> do
          let path = ss </> "credentials.json"
          configureCredentialsFile (SmithHomeCredentials path) path
    Just s ->
      configureCredentialsByteString EnvironmentCredentials . Text.encodeUtf8 . Text.pack $ s


configureCredentialsByteString :: SmithCredentialsType -> ByteString -> ExceptT SmithConfigureError IO SmithCredentials
configureCredentialsByteString t keydata = do
  json <- case Aeson.decodeStrict keydata of
    Nothing ->
      left $ SmithConfigureJsonDecodeError t
    Just v ->
      pure v

  jwk <- case Aeson.decodeStrict keydata of
    Nothing ->
      left $ SmithConfigureJwkDecodeError t
    Just v ->
      pure v

  issuer <- case Aeson.parse (Aeson.withObject "JWK" $ \o -> o .: "smith.st/identity-id") json of
    Aeson.Error _msg ->
      left $ SmithConfigureIdentityIdDecodeError t
    Aeson.Success v ->
      pure $ IdentityId . Text.pack . show $ (v :: Int64)

  pure $ SmithCredentials t issuer jwk


configureCredentialsFile :: SmithCredentialsType -> FilePath -> ExceptT SmithConfigureError IO SmithCredentials
configureCredentialsFile t path = do
  exists <- liftIO . Directory.doesFileExist $ path
  case exists of
    False ->
      left $ SmithConfigureCredentialsNotFound t
    True -> do
      bytes <- liftIO . ByteString.readFile $ path
      configureCredentialsByteString t bytes


data SmithConfigureError =
    SmithConfigureCredentialsNotFound SmithCredentialsType
  | SmithConfigureJsonDecodeError SmithCredentialsType
  | SmithConfigureJwkDecodeError SmithCredentialsType
  | SmithConfigureIdentityIdDecodeError SmithCredentialsType
    deriving (Eq, Ord, Show)


renderSmithCredentialsType :: SmithCredentialsType -> Text
renderSmithCredentialsType t =
  case t of
    EnvironmentCredentials ->
      "Credentials were found in environment using $SMITH_JWK."
    SmithHomeCredentials path ->
      mconcat ["Credentials were found using $SMITH_HOME: ", Text.pack path]
    HomeCredentials path ->
      mconcat ["Credentials were found using $HOME: ", Text.pack path]
    SuppliedCredentials ->
      "Credentials were supplied programatically."


renderSmithConfigureError :: SmithConfigureError -> Text
renderSmithConfigureError e =
  case e of
    SmithConfigureCredentialsNotFound t ->
      mconcat ["Smith credentials not found. ", case t of
        SuppliedCredentials ->
          "It looks you are using the library programatically, consider using 'configure'."
        SmithHomeCredentials path ->
          mconcat ["$SMITH_HOME is set, using $SMITH_HOME/credentials.json, but credentials file was not found: ", Text.pack path]
        HomeCredentials path ->
          mconcat ["$SMITH_HOME is not set, defaulting to $HOME/.smith/credentials.json, but credentials file was not found: ", Text.pack path]
        EnvironmentCredentials ->
          "Attempted to use $SMITH_JWK, but it was not set."]
    SmithConfigureJsonDecodeError t ->
      mconcat ["Smith credentials are not valid json and could not be decoded. ", renderSmithCredentialsType t]
    SmithConfigureJwkDecodeError t ->
      mconcat ["Smith credentials do not contain a valid JWT and could not be decoded. ", renderSmithCredentialsType t]
    SmithConfigureIdentityIdDecodeError t ->
      mconcat ["Smith credentials do not contain a valid identity and could not be decoded. ", renderSmithCredentialsType t]


left :: Monad m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
