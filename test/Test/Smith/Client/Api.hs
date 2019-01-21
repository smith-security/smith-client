{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Smith.Client.Api where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (runExceptT)
import           Hedgehog

import qualified Smith.Client as Smith
import           Smith.Client.Data.Environment
import           Smith.Client.Data.CertificateRequest

import qualified System.Environment as Environment


prop_end_to_end :: Property
prop_end_to_end =
  withTests 1 . property $ do
    e <- liftIO $ Environment.lookupEnv "SMITH_ENDPOINT"
    case e of
      Nothing ->
        success
      Just _endpoint -> do
        smith <- (=<<) evalEither . liftIO . runExceptT $
          Smith.configureT
        _userinfo <- (=<<) evalEither . liftIO $
          Smith.runRequest smith Smith.userinfo
        _cas <- (=<<) evalEither . liftIO $
          Smith.runRequest smith (Smith.keys $ Environment "muppets")
        _cert <- (=<<) evalEither . liftIO $
          Smith.runRequest smith (Smith.issue $
            CertificateRequest (PublicKey "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJRDZnxY1ywG2x5Mn2Ndlr7noCDwuKJPSVCy9LHNPauvPubXNVLDBV9ygkUphP+A9rtPQQ6ziGrG0NkhlGQ8beSfLq+MF283EnnhAnxnzqyvFQFh0seHo1a+MOgGcUliLbQEKE5uzt4An1epxhRyF71DmnNtASUh2TraTHcBqWCejr6INzv3dy4QeaDIUUI/C3xWqlDev+IFszTHS/2w3WPPMaJQUIeHN4MxUx+lDDIIIIagF03KM+k3AGZn+4j8WgNp916gjKXBvzX3gE11BBRNojq1N0uICDBYl5Si6uQzt46PLB0IojQ1zpQQREX0GKdd40Ar9ZuXOQlcLnBEs7 example@example.com") [Principal "root"] (Environment "muppets") (HostName "example"))
        success
    success

tests :: IO Bool
tests =
  checkParallel $$(discover)
