module GenerateMLMSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAll)

import Config (Config(..), reasonableConfigs)

import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.Types (valid)

spec :: Spec
spec = do
    describe "generateMLM" $ do
        it "creates valid MLMs" $
          forAll reasonableConfigs $ \Config{..} ->
          forAll (generateMLM
                  projectNameString
                  maxLvl0
                  numClasses0
                  numAssociations0
                  chanceToNotConcretize
                  chanceToNotInherit
                  numAttributes0
                  multSpecsAttributes0
                  precisionFactorAttributes0
                  multSpecsAssociations0
                  visibilityChanceAssociations) $
          valid ()
