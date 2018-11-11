module Smith.Client.Data.User (
    UserInfo (..)
  ) where

import           Data.Int (Int64)

newtype UserInfo =
    UserInfo {
        userInfo :: Int64
      } deriving (Eq, Ord, Show)
