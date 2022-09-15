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
anMLM = MLM
    { name = Name "imported_from_testing"
    , classes =
        [ Class
            { isAbstract = False
            , level = 4
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes =
                [ Attribute
                    { level = 3
                    , name = Name "attribute7"
                    , dataType = Date
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                , Attribute
                    { level = 3
                    , name = Name "attribute2"
                    , dataType = MonetaryValue
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                ]
            , operations = []
            , slots = []
            }
        , Class
            { isAbstract = False
            , level = 3
            , name = Name "B"
            , parents = []
            , classifier = Just
                ( Name "A" )
            , attributes =
                [ Attribute
                    { level = 2
                    , name = Name "attribute8"
                    , dataType = Complex
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                , Attribute
                    { level = 2
                    , name = Name "attribute3"
                    , dataType = Complex
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                ]
            , operations = []
            , slots =
                [ Slot
                    { name = Name "attribute7"
                    , value = VDate
                        ( 2000
                        , 1
                        , 1
                        )
                    }
                , Slot
                    { name = Name "attribute2"
                    , value = VMonetaryValue
                        ( "9.99 "
                        , "apples"
                        )
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 2
            , name = Name "C"
            , parents = []
            , classifier = Just
                ( Name "B" )
            , attributes =
                [ Attribute
                    { level = 1
                    , name = Name "attribute9"
                    , dataType = Boolean
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                , Attribute
                    { level = 1
                    , name = Name "attribute4"
                    , dataType = Boolean
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                ]
            , operations = []
            , slots =
                [ Slot
                    { name = Name "attribute8"
                    , value = VComplex "null"
                    }
                , Slot
                    { name = Name "attribute3"
                    , value = VComplex "null"
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 1
            , name = Name "D"
            , parents = []
            , classifier = Just
                ( Name "C" )
            , attributes =
                [ Attribute
                    { level = 0
                    , name = Name "attribute10"
                    , dataType = Element
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                , Attribute
                    { level = 0
                    , name = Name "attribute5"
                    , dataType = AuxiliaryClass
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                ]
            , operations = []
            , slots =
                [ Slot
                    { name = Name "attribute9"
                    , value = VBoolean False
                    }
                , Slot
                    { name = Name "attribute4"
                    , value = VBoolean True
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 0
            , name = Name "E"
            , parents = []
            , classifier = Just
                ( Name "D" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { name = Name "attribute10"
                    , value = VElement "null"
                    }
                , Slot
                    { name = Name "attribute5"
                    , value = VAuxiliaryClass "null"
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 1
            , name = Name "F"
            , parents = []
            , classifier = Just
                ( Name "C" )
            , attributes =
                [ Attribute
                    { level = 0
                    , name = Name "attribute6"
                    , dataType = Currency
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                ]
            , operations = []
            , slots =
                [ Slot
                    { name = Name "attribute9"
                    , value = VBoolean True
                    }
                , Slot
                    { name = Name "attribute4"
                    , value = VBoolean True
                    }
                ]
            }
        ]
    , associations =
        [ Association
            { name = Name "a"
            , source = Name "A"
            , target = Name "B"
            , lvlSource = 2
            , lvlTarget = 2
            , multSource = Multiplicity
                ( 0
                , Just 2
                )
            , multTarget = Multiplicity
                ( 0
                , Just 1
                )
            , visibleSource = True
            , visibleTarget = True
            }
        , Association
            { name = Name "b"
            , source = Name "B"
            , target = Name "D"
            , lvlSource = 1
            , lvlTarget = 0
            , multSource = Multiplicity
                ( 0
                , Just 2
                )
            , multTarget = Multiplicity
                ( 0
                , Just 2
                )
            , visibleSource = True
            , visibleTarget = True
            }
        ]
    , links =
        [ Link
            { name = Name "a"
            , source = Name "C"
            , target = Name "C"
            }
        , Link
            { name = Name "b"
            , source = Name "D"
            , target = Name "E"
            }
        , Link
            { name = Name "b"
            , source = Name "F"
            , target = Name "E"
            }
        ]
    }
