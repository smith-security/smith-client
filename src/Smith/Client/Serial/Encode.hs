-- |
-- Smith JSON representiation encoders.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Serial.Encode (
    certificateRequest
  ) where


import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

import           Smith.Client.Data.CertificateRequest
import           Smith.Client.Data.Environment


certificateRequest :: CertificateRequest -> Aeson.Value
certificateRequest request =
  Aeson.object [
      "public-key" .= (getPublicKey $ certificateRequestPublicKey request)
    , "principals" .= (getPrincipal <$> certificateRequestPrincipals request)
    , "environment" .= (getEnvironment $ certificateRequestEnvironment request)
    , "host-name" .= (getHostName <$> certificateRequestHostName request)
    ]
