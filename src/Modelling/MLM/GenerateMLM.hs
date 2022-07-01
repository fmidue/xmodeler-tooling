module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types

-- numClasses numAssociations
generateMLM :: String -> Int -> Int -> Int -> MLM
generateMLM mlmName _ _ _ =
    MLM mlmName [] [] []
