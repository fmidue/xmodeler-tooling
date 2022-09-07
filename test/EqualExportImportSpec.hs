module EqualExportImportSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty, forAll)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Data.GraphViz (GraphvizCommand(..))
import Config (reasonableConfigs)
import Modelling.MLM.Types
import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.ToXModeler (toXModeler)
import Modelling.MLM.GenerateMLM (generateMLM)

spec :: Spec
spec = do
         describe "Exporting an MLM and then importing it and comparing to the original." $
           modifyMaxSuccess (const 5) $
            it "correctly judges some randomly generated MLM to be equal to the same MLM after being exported and then imported. \n So, importing and exporting an MLM does not change it." $
                forAll reasonableConfigs $ \config ->
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
         describe "Roundtripping of exporting and importing an MLM" $
          it "works on a specific MLM that contains an operation" $
            ioProperty $ do
              let file = "should_pass/exporting_operations.hs"
              mlm <- (read :: String -> MLM) <$> readFile file
              writeFile "testing.xml" =<< toXModeler (Neato, (**1.2), 1.1, 163) mlm
              importedMLM <- fromXModeler "testing.xml"
              return $ (mlm, importedMLM) `shouldSatisfy` uncurry (==)

anMLM :: MLM
anMLM = MLM {name = Name "randomMLM", classes = [Class {isAbstract = False, level = 3, name = Name "A", parents = [], classifier = Nothing, attributes = [Attribute {level = 0, name = Name "attr1", dataType = AuxiliaryClass, multiplicity = Multiplicity (0,Just 1)},Attribute {level = 1, name = Name "attr2", dataType = Integer, multiplicity = Multiplicity (0,Just 1)},Attribute {level = 2, name = Name "attr3", dataType = Currency, multiplicity = Multiplicity (0,Nothing)}], operations = [], slots = []},Class {isAbstract = False, level = 2, name = Name "B", parents = [], classifier = Just (Name "A"), attributes = [Attribute {level = 0, name = Name "attr4", dataType = Float, multiplicity = Multiplicity (0,Nothing)},Attribute {level = 1, name = Name "attr5", dataType = AuxiliaryClass, multiplicity = Multiplicity (0,Nothing)}], operations = [], slots = [Slot {name = Name "attr3", value = VCurrency USD}]},Class {isAbstract = False, level = 1, name = Name "C", parents = [], classifier = Just (Name "B"), attributes = [Attribute {level = 0, name = Name "attr6", dataType = String, multiplicity = Multiplicity (0,Just 1)}], operations = [], slots = [Slot {name = Name "attr2", value = VInteger 1},Slot {name = Name "attr5", value = VAuxiliaryClass "null"}]},Class {isAbstract = False, level = 0, name = Name "D", parents = [], classifier = Just (Name "C"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 6.38},Slot {name = Name "attr6", value = VString "some String value"}]},Class {isAbstract = False, level = 2, name = Name "E", parents = [Name "B"], classifier = Just (Name "A"), attributes = [Attribute {level = 0, name = Name "attr7", dataType = MonetaryValue, multiplicity = Multiplicity (0,Just 1)},Attribute {level = 1, name = Name "attr8", dataType = String, multiplicity = Multiplicity (0,Just 1)}], operations = [], slots = [Slot {name = Name "attr3", value = VCurrency EUR}]},Class {isAbstract = False, level = 1, name = Name "F", parents = [], classifier = Just (Name "B"), attributes = [Attribute {level = 0, name = Name "attr9", dataType = AuxiliaryClass, multiplicity = Multiplicity (0,Just 2)}], operations = [], slots = [Slot {name = Name "attr2", value = VInteger 4},Slot {name = Name "attr5", value = VAuxiliaryClass "null"}]},Class {isAbstract = False, level = 1, name = Name "G", parents = [], classifier = Nothing, attributes = [Attribute {level = 0, name = Name "attr10", dataType = Float, multiplicity = Multiplicity (0,Just 2)}], operations = [], slots = []},Class {isAbstract = False, level = 1, name = Name "H", parents = [], classifier = Nothing, attributes = [Attribute {level = 0, name = Name "attr11", dataType = String, multiplicity = Multiplicity (0,Nothing)}], operations = [], slots = []},Class {isAbstract = False, level = 1, name = Name "I", parents = [Name "C",Name "F"], classifier = Just (Name "B"), attributes = [Attribute {level = 0, name = Name "attr12", dataType = Boolean, multiplicity = Multiplicity (0,Just 2)}], operations = [], slots = [Slot {name = Name "attr2", value = VInteger 10},Slot {name = Name "attr5", value = VAuxiliaryClass "null"}]},Class {isAbstract = False, level = 0, name = Name "J", parents = [], classifier = Just (Name "I"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 6.26},Slot {name = Name "attr6", value = VString "some String value"},Slot {name = Name "attr12", value = VBoolean False}]},Class {isAbstract = False, level = 0, name = Name "K", parents = [], classifier = Just (Name "F"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 0.84},Slot {name = Name "attr9", value = VAuxiliaryClass "null"}]},Class {isAbstract = False, level = 0, name = Name "L", parents = [], classifier = Just (Name "C"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 0.1},Slot {name = Name "attr6", value = VString "some String value"}]},Class {isAbstract = False, level = 0, name = Name "M", parents = [], classifier = Just (Name "G"), attributes = [], operations = [], slots = [Slot {name = Name "attr10", value = VFloat 3.28}]},Class {isAbstract = False, level = 1, name = Name "N", parents = [], classifier = Just (Name "E"), attributes = [Attribute {level = 0, name = Name "attr13", dataType = Date, multiplicity = Multiplicity (0,Just 1)}], operations = [], slots = [Slot {name = Name "attr2", value = VInteger 2},Slot {name = Name "attr5", value = VAuxiliaryClass "null"},Slot {name = Name "attr8", value = VString "some String value"}]},Class {isAbstract = False, level = 1, name = Name "O", parents = [Name "C",Name "F"], classifier = Just (Name "B"), attributes = [Attribute {level = 0, name = Name "attr14", dataType = String, multiplicity = Multiplicity (0,Nothing)}], operations = [], slots = [Slot {name = Name "attr2", value = VInteger 1},Slot {name = Name "attr5", value = VAuxiliaryClass "null"}]},Class {isAbstract = False, level = 0, name = Name "P", parents = [], classifier = Just (Name "N"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 6.33},Slot {name = Name "attr7", value = VMonetaryValue ("9.99 ","apples")},Slot {name = Name "attr13", value = VDate (2000,1,1)}]},Class {isAbstract = False, level = 0, name = Name "Q", parents = [], classifier = Just (Name "I"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 9.67},Slot {name = Name "attr6", value = VString "some String value"},Slot {name = Name "attr12", value = VBoolean False}]},Class {isAbstract = False, level = 0, name = Name "R", parents = [Name "P"], classifier = Just (Name "N"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 1.25},Slot {name = Name "attr7", value = VMonetaryValue ("9.99 ","apples")},Slot {name = Name "attr13", value = VDate (2000,1,1)}]},Class {isAbstract = False, level = 3, name = Name "S", parents = [Name "A"], classifier = Nothing, attributes = [Attribute {level = 0, name = Name "attr15", dataType = MonetaryValue, multiplicity = Multiplicity (0,Just 1)},Attribute {level = 1, name = Name "attr16", dataType = Date, multiplicity = Multiplicity (0,Nothing)},Attribute {level = 2, name = Name "attr17", dataType = Integer, multiplicity = Multiplicity (0,Just 1)}], operations = [], slots = []},Class {isAbstract = False, level = 0, name = Name "T", parents = [], classifier = Just (Name "H"), attributes = [], operations = [], slots = [Slot {name = Name "attr11", value = VString "some String value"}]},Class {isAbstract = False, level = 2, name = Name "U", parents = [], classifier = Nothing, attributes = [Attribute {level = 0, name = Name "attr18", dataType = Integer, multiplicity = Multiplicity (0,Nothing)},Attribute {level = 1, name = Name "attr19", dataType = Element, multiplicity = Multiplicity (0,Nothing)}], operations = [], slots = []},Class {isAbstract = False, level = 1, name = Name "V", parents = [], classifier = Just (Name "U"), attributes = [Attribute {level = 0, name = Name "attr20", dataType = Float, multiplicity = Multiplicity (0,Just 1)}], operations = [], slots = [Slot {name = Name "attr19", value = VElement "null"}]},Class {isAbstract = False, level = 0, name = Name "W", parents = [], classifier = Just (Name "G"), attributes = [], operations = [], slots = [Slot {name = Name "attr10", value = VFloat 6.8}]},Class {isAbstract = False, level = 0, name = Name "X", parents = [Name "J"], classifier = Just (Name "I"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 1.49},Slot {name = Name "attr6", value = VString "some String value"},Slot {name = Name "attr12", value = VBoolean True}]},Class {isAbstract = False, level = 1, name = Name "Y", parents = [Name "H"], classifier = Nothing, attributes = [Attribute {level = 0, name = Name "attr21", dataType = AuxiliaryClass, multiplicity = Multiplicity (0,Nothing)}], operations = [], slots = []},Class {isAbstract = False, level = 0, name = Name "Z", parents = [], classifier = Just (Name "H"), attributes = [], operations = [], slots = [Slot {name = Name "attr11", value = VString "some String value"}]},Class {isAbstract = False, level = 0, name = Name "AA", parents = [], classifier = Just (Name "C"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 0.95},Slot {name = Name "attr6", value = VString "some String value"}]},Class {isAbstract = False, level = 0, name = Name "AB", parents = [], classifier = Just (Name "O"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 4.97},Slot {name = Name "attr6", value = VString "some String value"},Slot {name = Name "attr14", value = VString "some String value"}]},Class {isAbstract = False, level = 0, name = Name "AC", parents = [], classifier = Just (Name "O"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 2.25},Slot {name = Name "attr6", value = VString "some String value"},Slot {name = Name "attr14", value = VString "some String value"}]},Class {isAbstract = False, level = 0, name = Name "AD", parents = [Name "AB"], classifier = Just (Name "O"), attributes = [], operations = [], slots = [Slot {name = Name "attr1", value = VAuxiliaryClass "null"},Slot {name = Name "attr4", value = VFloat 6.67},Slot {name = Name "attr6", value = VString "some String value"},Slot {name = Name "attr14", value = VString "some String value"}]}], associations = [Association {name = Name "a", source = Name "S", target = Name "V", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "b", source = Name "B", target = Name "S", lvlSource = 0, lvlTarget = 1, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "c", source = Name "U", target = Name "N", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "d", source = Name "E", target = Name "V", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "e", source = Name "V", target = Name "A", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "f", source = Name "F", target = Name "C", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "g", source = Name "Y", target = Name "E", lvlSource = 0, lvlTarget = 1, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "h", source = Name "N", target = Name "U", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = True},Association {name = Name "i", source = Name "B", target = Name "N", lvlSource = 1, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "j", source = Name "V", target = Name "A", lvlSource = 0, lvlTarget = 2, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "k", source = Name "A", target = Name "I", lvlSource = 1, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "l", source = Name "I", target = Name "O", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "m", source = Name "U", target = Name "I", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "n", source = Name "G", target = Name "I", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "o", source = Name "C", target = Name "Y", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "p", source = Name "V", target = Name "V", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "q", source = Name "Y", target = Name "H", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = True},Association {name = Name "r", source = Name "I", target = Name "F", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "s", source = Name "C", target = Name "F", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "t", source = Name "O", target = Name "G", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "u", source = Name "H", target = Name "V", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "v", source = Name "C", target = Name "O", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = True},Association {name = Name "w", source = Name "I", target = Name "I", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "x", source = Name "V", target = Name "F", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = True},Association {name = Name "y", source = Name "C", target = Name "N", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = True},Association {name = Name "z", source = Name "N", target = Name "A", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "aa", source = Name "A", target = Name "A", lvlSource = 0, lvlTarget = 1, multTargetToSource = Multiplicity (0,Nothing), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "ab", source = Name "V", target = Name "V", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "ac", source = Name "V", target = Name "N", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Nothing), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "ad", source = Name "O", target = Name "H", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "ae", source = Name "N", target = Name "S", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "af", source = Name "Y", target = Name "H", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False},Association {name = Name "ag", source = Name "Y", target = Name "B", lvlSource = 0, lvlTarget = 0, multTargetToSource = Multiplicity (0,Just 1), multSourceToTarget = Multiplicity (0,Just 1), sourceVisibleFromTarget = False, targetVisibleFromSource = False}], links = []}
