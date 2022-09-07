module GenerateMLMSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAll)

import Config (reasonableConfigs)

import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.Types (valid, MLM(..))
import Modelling.MLM.Config (Config(..))

spec :: Spec
spec = do
        describe "generateMLM" $
          it "creates valid MLMs" $
            forAll reasonableConfigs $ \config ->
            forAll (generateMLM config) $
            valid ()
        describe "generateMLM" $
          it "creates as many classes as asked for" $
            forAll reasonableConfigs $ \config@Config{numClasses0} ->
            forAll (generateMLM config) $ \MLM{classes} ->
            length classes == numClasses0
