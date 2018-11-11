module Smith.Client.Response (
    Responder (..)

  , json
  ) where

import           Data.Aeson (Value)
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as Lazy

import qualified Network.HTTP.Client as HTTP


newtype Responder a =
  Responder {
      runResponder :: HTTP.Response Lazy.ByteString -> a
    }

json :: Int -> (Value -> Parser a) -> Responder a
json code parser =
  Responder $ \res ->
    error "todo"
