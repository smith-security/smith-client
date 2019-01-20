module Smith.Client.Data.Identity (
    IdentityId (..)
  ) where

import           Data.Text (Text)


newtype IdentityId =
    IdentityId {
        identityId :: Text
      } deriving (Eq, Ord, Show)
