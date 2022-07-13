{-# LANGUAGE NamedFieldPuns #-}


module Modelling.MLM.GenerateMLM (generateMLM) where

-- import Modelling.MLM.Types
{-

putValid :: a -> b -> a
putValid a b = let
    c = put a b in
        if valid () c then c else a

generateMLM :: String -> Int -> Int -> Int -> MLM
generateMLM mlmName _ _ _ =
    let
        myMlm = MLM mlmName [] [] []
        myClass1 = Class False 2 "A" [] Nothing [] [] []
        myClass2 = Class False 2 "B" [myClass1] Nothing [] [] []
    in
        put (put myMlm myClass1) myClass2



-}

generateMLM :: String
generateMLM = ""
