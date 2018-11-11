-- |
-- Smith API definition response combinators.
--
-- You shouldn't need these unless defining custom/additional calls, see
-- 'Smith.Client.Api' for complete API definition.
--
module Smith.Client.Response (
    ResponseError (..)

  , Responder (..)

  , json
  ) where

import           Data.Aeson (Value)
import           Data.Aeson.Types (Parser)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import           Smith.Client.Error
import qualified Smith.Client.Serial.Decode as Decode


data ResponseError =
    ParseResponseError Int Lazy.ByteString Text
  | UnknownStatusResponseError Int Lazy.ByteString
    deriving (Eq, Ord, Show)

newtype Responder a =
  Responder {
      runResponder :: HTTP.Response Lazy.ByteString -> Either SmithError a
    }

json :: Int -> (Value -> Parser a) -> Responder a
json code parser =
  Responder $ \res ->
    case (HTTP.statusCode . HTTP.responseStatus) res of
      400 ->
        (first (SmithResponseParseError 400 (HTTP.responseBody res)) $
          Decode.parse Decode.errored (HTTP.responseBody res)) >>= Left
      403 ->
        (first (SmithResponseParseError 403 (HTTP.responseBody res)) $
          Decode.parse Decode.forbidden (HTTP.responseBody res)) >>= Left
      x | x == code ->
        first (SmithResponseParseError x (HTTP.responseBody res)) $
          Decode.parse parser (HTTP.responseBody res)
      x ->
        Left $
          SmithStatusCodeError x (HTTP.responseBody res)
