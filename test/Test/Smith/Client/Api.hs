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
import qualified System.IO as IO

prop_end_to_end :: Property
prop_end_to_end =
  withTests 1 . property $ do
    e <- liftIO $ Environment.lookupEnv "SMITH_ENDPOINT"
    case e of
      Nothing ->
        success
      Just _endpoint -> do
        smith <- (=<<) evalEither . liftIO . runExceptT $
          Smith.configure
        userinfo <- (=<<) evalEither . liftIO $
          Smith.runRequest smith Smith.userinfo
        liftIO . IO.print $ userinfo
        cas <- (=<<) evalEither . liftIO $
          Smith.runRequest smith (Smith.keys $ Environment "muppets")
        liftIO . IO.print $ cas
        cert <- (=<<) evalEither . liftIO $
          Smith.runRequest smith (Smith.issue $
            CertificateRequest (PublicKey "key") [Principal "root"] (Environment "muppets") (HostName "example"))
        liftIO . IO.print $ cert
        success
    success

tests :: IO Bool
tests =
  checkParallel $$(discover)
