module Smith.Client.Data.CertificateRequest (
    Environment (..)
  , Principal (..)
  , PublicKey (..)
  , HostName (..)
  , CertificateRequest (..)
  ) where


import           Data.Text (Text)


newtype Environment =
  Environment {
      getEnvironment :: Text
    } deriving (Eq, Ord, Show)

newtype Principal =
  Principal {
      getPrincipal :: Text
    } deriving (Eq, Ord, Show)

newtype PublicKey =
  PublicKey {
      getPublicKey :: Text
    } deriving (Eq, Ord, Show)

newtype HostName =
  HostName {
      getHostName :: Text
    } deriving (Eq, Ord, Show)

data CertificateRequest =
  CertificateRequest {
      certificateRequestPublicKey :: PublicKey
    , certificateRequestPrincipals :: [Principal]
    , certificateRequestEnvironment :: Environment
    , certificateRequestHostName :: HostName
    } deriving (Eq, Ord, Show)
