module GenerateMLMSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAll)

import Config (reasonableConfigs)

import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.Types (valid, MLM(..), Class(..))
import Modelling.MLM.Config (Config(..))

spec :: Spec
spec = do
        describe "generateMLM" $
          it "creates valid MLMs" $
            forAll reasonableConfigs $ \config ->
            forAll (generateMLM config) $
            valid ()
        describe "generateMLM" $
          it "respects certain configuration parameters" $
            forAll reasonableConfigs $ \config@Config{maxLvl0, numClasses0, numAssociations0} ->
            forAll (generateMLM config) $ \MLM{classes, associations} ->
              maximum (map (\Class{level} -> level) classes) <= maxLvl0 &&
              length classes == numClasses0 &&
              length associations == numAssociations0
