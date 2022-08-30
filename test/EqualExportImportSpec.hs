module EqualExportImportSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty, forAll)

import Data.GraphViz (GraphvizCommand(..))
import Config (Config(..), reasonableConfigs)
import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.ToXModeler (toXModeler)
import Modelling.MLM.GenerateMLM (generateMLM)

spec :: Spec
spec = describe "Exporting an MLM and then importing it and comparing to the original." $
            it "correctly judges some randomly generated MLM to be equal to the same MLM after being exported and then imported. \n So, importing and exporting an MLM does not change it." $
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
                    \randomMLM -> ioProperty $ do
                        x <- toXModeler (Neato, (**1.135), 1.1, 163) randomMLM
                        writeFile "exportedForTesting.xml" x
                        importedMLM <- fromXModeler "exportedForTesting.xml"
                        return $ (randomMLM, importedMLM) `shouldSatisfy` uncurry (==)
