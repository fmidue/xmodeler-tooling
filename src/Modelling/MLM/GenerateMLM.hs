module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types

class CanBeInsertedInto a b where
    insert :: a -> b -> a

instance CanBeInsertedInto MLM Class where
    insert mlm class' = mlm {classes = class' : classes mlm}

generateMLM :: String -> Int -> Int -> Int -> MLM
generateMLM mlmName _ _ _ =
    let
        myMlm = MLM mlmName [] [] []
        myClass1 = Class False 2 "A" [] Nothing [] [] []
        myClass2 = Class False 2 "B" [myClass1] Nothing [] [] []
    in
        insert (insert myMlm myClass1) myClass2



