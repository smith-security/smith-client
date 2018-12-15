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


userinfo :: Request UserInfo
userinfo =
  Request HTTP.GET "userinfo"
    (Response.json 200 Decode.userinfo)
    Request.none

issue :: CertificateRequest -> Request Certificate
issue request =
  Request HTTP.POST "issue"
    (Response.json 200 Decode.certificate)
    (Request.json $ Encode.certificateRequest request)

keys :: Environment -> Request [AuthorityPublicKey]
keys environment =
  Request HTTP.GET (mconcat ["environment/public-keys/", getEnvironment environment])
    (Response.json 200 Decode.authorityPublicKey)
    Request.none
