module Smith.Client.Data.Environment (
    Environment (..)
  ) where


import           Data.Text (Text)


newtype Environment =
  Environment {
      getEnvironment :: Text
    } deriving (Eq, Ord, Show)
