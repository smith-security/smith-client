module Smith.Client.Data.Error (
    SmithError (..)
  , ErrorCode (..)
  , ErrorMessage (..)
  ) where

import           Data.Text (Text)


data SmithError =
    SmithApplicationError ErrorCode (Maybe ErrorMessage)
  | SmithAuthentictionError

newtype ErrorCode =
  ErrorCode {
      getErrorCode :: Text
    } deriving (Eq, Ord, Show)

newtype ErrorMessage =
  ErrorMessage {
      getErrorMessage :: Text
    } deriving (Eq, Ord, Show)
