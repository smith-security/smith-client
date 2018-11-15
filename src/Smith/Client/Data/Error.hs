module Smith.Client.Data.Error (
    SmithError (..)
  , ErrorCode (..)
  , ErrorMessage (..)
  ) where

import           Data.Text (Text)
import qualified Data.ByteString.Lazy as Lazy
import qualified Network.OAuth2.JWT.Client as OAuth2

data SmithError =
    SmithApplicationError ErrorCode (Maybe ErrorMessage)
  | SmithAuthorizationError ErrorCode (Maybe ErrorMessage)
  | SmithAuthenticationError OAuth2.GrantError
  | SmithResponseParseError Int Lazy.ByteString Text
  | SmithStatusCodeError Int Lazy.ByteString
  | SmithUrlParseError Text
    deriving (Eq, Show)

newtype ErrorCode =
  ErrorCode {
      getErrorCode :: Text
    } deriving (Eq, Ord, Show)

newtype ErrorMessage =
  ErrorMessage {
      getErrorMessage :: Text
    } deriving (Eq, Ord, Show)
