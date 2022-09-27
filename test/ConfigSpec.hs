module ConfigSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAll)
import Data.Maybe (isNothing)

import Config (reasonableConfigs, smallConfigs)
import Modelling.MLM.Config (checkConfig, defaultConfig)

spec :: Spec
spec = do
  describe "checkConfig" $ do
    it "should accept the default config" $
      isNothing (checkConfig defaultConfig)
    it "should accept valid bounds" $
      forAll reasonableConfigs (isNothing . checkConfig)
    it "should accept valid bounds" $
      forAll smallConfigs (isNothing . checkConfig)
