{-# LANGUAGE TemplateHaskell #-}
module Test.Smith.Client.Api where

import           Hedgehog

import qualified Smith.Client.Api as Api


prop_placeholder :: Property
prop_placeholder =
  property $
    1 === 1

tests :: IO Bool
tests =
  checkParallel $$(discover)
