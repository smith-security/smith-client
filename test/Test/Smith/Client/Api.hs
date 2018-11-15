{-# LANGUAGE TemplateHaskell #-}
module Test.Smith.Client.Api where

import           Control.Monad.IO.Class (MonadIO (..))

import           Hedgehog

import qualified Smith.Client as Smith

import qualified System.Environment as Environment

prop_end_to_end :: Property
prop_end_to_end =
  withTests 1 . property $ do
    e <- liftIO $ Environment.lookupEnv "SMITH_ENDPOINT"
    case e of
      Nothing ->
        success
      Just _endpoint -> do
        _userinfo <- (=<<) evalEither . liftIO $ do
          smith <- Smith.configure
          Smith.runRequest smith Smith.userinfo
        success
    success

tests :: IO Bool
tests =
  checkParallel $$(discover)
