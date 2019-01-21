-- |
-- Smith certificate authority data types.
--
module Smith.Client.Data.CertificateAuthority (
    AuthorityPublicKey (..)
  ) where

import           Data.Text (Text)


newtype AuthorityPublicKey =
  AuthorityPublicKey {
      getAuthorityPublicKey :: Text
    } deriving (Eq, Ord, Show)
