-- |
-- Smith API definitions.
--
-- Should be used as a qualified import from top-level module:
--
-- > import qualified Smith.Client as Smith
--
-- Example:
-- > Smith.runRequest configuration Smith.userinfo
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Api (
    userinfo
  , issue
  , keys
  ) where

import qualified Network.HTTP.Types as HTTP

import           Smith.Client.Data.Certificate
import           Smith.Client.Data.CertificateAuthority
import           Smith.Client.Data.CertificateRequest
import           Smith.Client.Data.Environment
import           Smith.Client.Data.User
import qualified Smith.Client.Response as Response
import           Smith.Client.Request (Request (..))
import qualified Smith.Client.Request as Request
import qualified Smith.Client.Serial.Decode as Decode
import qualified Smith.Client.Serial.Encode as Encode


-- |
-- Obtain the identity information of the currently authenticated user or service.
--
userinfo :: Request UserInfo
userinfo =
  Request HTTP.GET "userinfo"
    (Response.json 200 Decode.userinfo)
    Request.none

-- |
-- Issue a certificate for the specified request details.
--
issue :: CertificateRequest -> Request Certificate
issue request =
  Request HTTP.POST "issue"
    (Response.json 200 Decode.certificate)
    (Request.json $ Encode.certificateRequest request)

-- |
-- Obtain all CA public keys for the specified environment.
--
keys :: Environment -> Request [AuthorityPublicKey]
keys environment =
  Request HTTP.GET (mconcat ["environment/public-keys/", getEnvironment environment])
    (Response.json 200 Decode.authorityPublicKeys)
    Request.none
