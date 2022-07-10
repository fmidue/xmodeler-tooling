{-# LANGUAGE NamedFieldPuns #-}


module Modelling.MLM.GenerateMLM (generateMLM, (<<)) where

import Modelling.MLM.Types
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

class Modifiable a b where
    (<<) :: a -> b -> a

data SourceOrTarget = Source | Target

-------- MLM
instance Modifiable MLM Name where
    (<<) y x =
         y {mlmName = x}
instance Modifiable MLM Class where
    (<<) y@(MLM {mlmClasses}) x =
         y {mlmClasses = mlmClasses ++ [x]}
instance Modifiable MLM Association where
    (<<) y@(MLM {mlmAssociations}) x =
         y {mlmAssociations = mlmAssociations ++ [x]}
instance Modifiable MLM Link where
    (<<) y@(MLM {mlmLinks}) x =
         y {mlmLinks = mlmLinks ++ [x]}

-------- Class
instance Modifiable Class Bool where
    (<<) y x =
         y {cIsAbstract = x}
instance Modifiable Class Level where
    (<<) y x =
         y {cLevel = x}
instance Modifiable Class Class where
    (<<) y@(Class {cParents}) x =
         y {cParents = cParents ++ [x]}
instance Modifiable Class (Maybe Class) where
    (<<) y x =
         y {cOf = x}
instance Modifiable Class Attribute where
    (<<) y@(Class {cAttributes}) x =
         y {cAttributes = cAttributes ++ [x {tClass = y}]}
instance Modifiable Class Operation where
    (<<) y@(Class {cOperations}) x =
         y {cOperations = cOperations ++ [x {oClass = y}]}
instance Modifiable Class Slot where
    (<<) y@(Class {cSlots}) x =
         y {cSlots = cSlots ++ [x {sClass = y}]}

-------- Attribute
instance Modifiable Attribute Level where
    (<<) y x =
         y {tLevel = x}
instance Modifiable Attribute Name where
    (<<) y x =
         y {tName = x}
instance Modifiable Attribute Type where
    (<<) y x =
         y {tType = x}
instance Modifiable Attribute Multiplicity where
    (<<) y x =
         y {tMultiplicity = x}

-------- Slot
instance Modifiable Slot Attribute where
    (<<) y x =
         y {sAttribute = x}
instance Modifiable Slot Type where
    (<<) y x =
         y {sValue = x}

-------- Operation
instance Modifiable Operation Int where
    (<<) y x =
         y {oLevel = x}
instance Modifiable Operation Name where
    (<<) y x =
         y {oName = x}
instance Modifiable Operation Type where
    (<<) y x =
         y {oType = x}
instance Modifiable Operation Bool where
    (<<) y x =
         y {oIsMonitored = x}
instance Modifiable Operation OperationBody where
    (<<) y x =
         y {oBody = x}

-------- Association
instance Modifiable Association (SourceOrTarget, Class) where
    (<<) y (direction, x) =
         case direction of
            Source -> y {aSource = x}
            Target -> y {aTarget = x}
instance Modifiable Association (SourceOrTarget, Level) where
    (<<) y (direction, x) =
         case direction of
            Source -> y {aLvlSource = x}
            Target -> y {aLvlTarget = x}
instance Modifiable Association (SourceOrTarget, Multiplicity) where
    (<<) y (direction, x) =
        case direction of
            Source -> y {aMultTargetToSource = x}
            Target -> y {aMultSourceToTarget = x}
instance Modifiable Association (SourceOrTarget, Bool) where
    (<<) y (direction, x) =
        case direction of
            Source -> y {aSourceVisibleFromTarget = x}
            Target -> y {aTargetVisibleFromSource = x}

-------- Link
instance Modifiable Link Association where
    (<<) y x =
         y {lAssociation = x}
instance Modifiable Link (SourceOrTarget, Class) where
    (<<) y (direction, x) =
        case direction of
            Source -> y {lSource = x}
            Target -> y {lTarget = x}

-------- Multiplicity
instance Modifiable Multiplicity (Int, Int) where
    (<<) y (x1, x2) =
        y {lower = x1, upper = x2}



generateMLM :: String
generateMLM = ""
