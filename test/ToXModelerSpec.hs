module ToXModelerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (forAll, ioProperty)

import Data.GraphViz (GraphvizCommand(..))
import Control.DeepSeq (force)

import Config (Config(..), reasonableConfigs)

import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.ToXModeler (toXModeler)

spec :: Spec
spec = describe "toXModeler" $
          it "can serialize MLMs to strings" $
            forAll reasonableConfigs $ \Config{..} ->
            forAll (generateMLM
                    projectNameString
                    maxLvl0
                    numClasses0
                    numAssociations0
                    chanceToNotConcretize
                    chanceToNotInherit
                    multSpecsAttributes0
                    multSpecsAssociations0
                    visibilityChanceAssociations) $
            \mlm -> ioProperty $ do
              output <- toXModeler (Neato, (**1.135), 1.1, 163) mlm
              return $ output `shouldSatisfy` not . null . force
