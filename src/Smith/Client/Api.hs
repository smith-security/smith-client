{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Api (
    userinfo
  , issue
  , keys
  ) where

import qualified Network.HTTP.Types as HTTP

import           Smith.Client.Data.User
import qualified Smith.Client.Response as Response
import           Smith.Client.Request (Request (..))
import qualified Smith.Client.Request as Request
import qualified Smith.Client.Serial.Decode as Decode


userinfo :: Request UserInfo
userinfo =
  Request HTTP.GET "userinfo"
    (Response.json 200 Decode.userinfo)
    Request.none

issue :: ()
issue =
  ()

keys :: ()
keys =
  ()
