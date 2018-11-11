{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Serial.Decode (
    userinfo
  , errored
  ) where

import           Data.Aeson (Value, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson

import           Smith.Client.Data.Error
import           Smith.Client.Data.User



userinfo :: Value -> Parser UserInfo
userinfo value =
  UserInfo <$> Aeson.parseJSON value

errored :: Value -> Parser SmithError
errored =
  Aeson.withObject "SmithError" $ \o ->
    SmithApplicationError
      <$> (ErrorCode <$> o .: "error")
      <*> (fmap ErrorMessage <$> o .:? "message")
