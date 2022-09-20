module EqualExportImportSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty, forAll)

import Data.GraphViz (GraphvizCommand(..))
import Config (smallConfigs)
import Modelling.MLM.Config (Config(..))
import Modelling.MLM.Types
import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.ToXModeler (toXModeler)
import Modelling.MLM.GenerateMLM (generateMLM)

spec :: Spec
spec = do
         describe "Exporting an MLM and then importing it and comparing to the original." $
            it "correctly judges some randomly generated MLM to be equal to the same MLM after being exported and then imported. \n So, importing and exporting an MLM does not change it." $
                forAll (fmap (\config -> config{numberOfClasses = 4}) smallConfigs) $ \config ->
                    forAll (generateMLM config) $
                    \randomMLM -> ioProperty $ do
                        x <- toXModeler (Neato, (**1.135), 1.1, 163) randomMLM
                        writeFile "exportedForTesting.xml" x
                        importedMLM <- fromXModeler "exportedForTesting.xml"
                        return $ (randomMLM, importedMLM) `shouldSatisfy` uncurry (==)
         describe "Roundtripping of exporting and importing an MLM" $
            it "works on a specific MLM" $
            let mlm = anMLM in ioProperty $ do
              writeFile "exportedForTesting.xml" =<< toXModeler (Neato, (**1.135), 1.1, 163) mlm
              importedMLM <- fromXModeler "exportedForTesting.xml"
              return $ (mlm, importedMLM) `shouldSatisfy` uncurry (==)

anMLM :: MLM
anMLM = MLM { name = Name "someMLM"
    , classes =
        [ Class
            { isAbstract = False
            , level = 1
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes = []
            , operations = [ Operation
                { level = 0
                , name = Name "myOperation"
                , dataType = Integer
                , isMonitored = True
                , body = "@Operation myOperation[monitor=true]():XCore::Integer&#10;  99&#10;end"
                }
            ]
            , slots = []
            }
        ]
    , associations = []
    , links = []
    }
