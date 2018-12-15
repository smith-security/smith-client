{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Serial.Decode (
    userinfo
  , certificate
  , errored
  , forbidden
  , parse
  ) where

import           Data.Aeson (Value, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)
import qualified Data.Text as Text

import           Smith.Client.Data.Certificate
import           Smith.Client.Data.Error
import           Smith.Client.Data.User


userinfo :: Value -> Parser UserInfo
userinfo =
  Aeson.withObject "UserInfo" $ \o ->
    UserInfo
      <$> o .: "sub"

certificate :: Value -> Parser Certificate
certificate =
  Aeson.withObject "Certificate" $ \o ->
    Certificate
      <$> o .: "certificate"

errored :: Value -> Parser SmithError
errored =
  Aeson.withObject "SmithError" $ \o ->
    SmithApplicationError
      <$> (ErrorCode <$> o .: "error")
      <*> (fmap ErrorMessage <$> o .:? "message")


forbidden :: Value -> Parser SmithError
forbidden =
  Aeson.withObject "SmithError" $ \o ->
    SmithAuthorizationError
      <$> (ErrorCode <$> o .: "error")
      <*> (fmap ErrorMessage <$> o .:? "message")


parse :: (Value -> Parser a) -> Lazy.ByteString -> Either Text a
parse to t =
  first Text.pack (Aeson.eitherDecode t) >>= \v -> case Aeson.parse to v of
    Aeson.Success a ->
      pure a
    Aeson.Error msg ->
      Left . Text.pack $ msg
