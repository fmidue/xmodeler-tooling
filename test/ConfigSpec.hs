module ConfigSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAll)
import Data.Maybe (isNothing, isJust)

import Config (reasonableConfigs, smallConfigs)
import Modelling.MLM.Config (checkConfig, defaultConfig, Config(..))

spec :: Spec
spec = do
  describe "checkConfig" $ do
    it "should accept the default config" $
      isNothing (checkConfig defaultConfig)
    it "should accept valid bounds" $
      forAll reasonableConfigs (isNothing . checkConfig)
    it "should accept valid bounds" $
      forAll smallConfigs (isNothing . checkConfig)
    it "should reject configs with numberOfClasses smaller than 0,  numberOfAssociations smaller than 0, maxClassLevel smaller than 1, numberOfAttributesPerConcretization smaller than 1, tendencies/chances/portions that are not in the range (0,1), multiplicitySpecAssociations second component smaller than 1, or an invalid MLM name" $ all (\x -> isJust (checkConfig x) || error (show x)) [
        defaultConfig{projectNameString = "abc?"},
        defaultConfig{projectNameString = "_abc"} ,
        defaultConfig{projectNameString = "123abc"},
        defaultConfig{maxClassLevel = 0},
        defaultConfig{numberOfClasses = -1},
        defaultConfig{numberOfAssociations = -2},
        defaultConfig{tendencyToConcretize = 2},
        defaultConfig{tendencyToInherit = -5},
        defaultConfig{multiplicitySpecAssociations = (5,5)},
        defaultConfig{multiplicitySpecAssociations = (0.4,-1)},
        defaultConfig{chanceVisibleAssociation = 2},
        defaultConfig{tendencyAbstractClass = -1},
        defaultConfig{portionOfPossibleLinksToKeep = 200},
        defaultConfig{numberOfAttributesPerConcretization = 0},
        defaultConfig{tendencyToDistanceAttributeFromItsInstantiation = -0.1}
      ]
