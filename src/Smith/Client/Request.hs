module Smith.Client.Request (
    Request (..)
  , Requester (..)

  , none
  ) where

import           Data.Text (Text)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

import           Smith.Client.Response (Responder (..))


data Request a =
  Request {
      requestMethod :: HTTP.StdMethod
    , requestPath :: Text
    , requestResponder :: Responder a
    , requestRequester :: Requester
    }


newtype Requester =
  Requester {
      runRequester :: HTTP.Request -> HTTP.Request
    }


none :: Requester
none =
  Requester id
