module Smith.Client.Data.Certificate (
    Certificate (..)
  ) where

import           Data.Text (Text)


newtype Certificate =
  Certificate {
      getCertificate :: Text
    } deriving (Eq, Ord, Show)
