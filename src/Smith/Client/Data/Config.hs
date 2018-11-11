module Smith.Client.Data.Config (
    Smith (..)
  ) where

import           Data.Text (Text)

import qualified Network.HTTP.Client as HTTP
import qualified Network.OAuth2.JWT.Client as OAuth2


newtype SmithEndpoint =
  SmithEndpoint {
      getSmithEndpoint :: Text
    } deriving (Eq, Ord, Show)

data Smith
  Smith SmithEndpoint HTTP.Manager OAuth2.Store
