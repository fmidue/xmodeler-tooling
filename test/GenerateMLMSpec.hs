module GenerateMLMSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAll)

import Config (reasonableConfigs)

import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.Types (valid)

spec :: Spec
spec = describe "generateMLM" $
          it "creates valid MLMs" $
            forAll reasonableConfigs $ \config ->
            forAll (generateMLM config) $
            valid ()
