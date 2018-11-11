-- |
-- Smith user data types.
--
module Smith.Client.Data.User (
    UserInfo (..)
  ) where

import           Data.Text (Text)


newtype UserInfo =
    UserInfo {
        userInfo :: Text
      } deriving (Eq, Ord, Show)
