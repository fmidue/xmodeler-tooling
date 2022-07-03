module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types

class Modifiable a b where
    put :: a -> b -> a

-- MLM
instance Modifiable MLM String where --projectName
    put mlm x = mlm {projectName = x}
instance Modifiable MLM Class where --class
    put mlm x = mlm {classes = x : classes mlm}
instance Modifiable MLM Association where
    put mlm x = mlm {associations = x : associations mlm}
instance Modifiable MLM Link where
    put mlm x = mlm {links = x : links mlm}

generateMLM :: String -> Int -> Int -> Int -> MLM
generateMLM mlmName _ _ _ =
-- Class
instance Modifiable Class Bool where
    put c x = c {isAbstract = x}
instance Modifiable Class Level where
    put c x = c {cLevel = x}
instance Modifiable Class String where
    put c x = c {cName = x}
instance Modifiable Class Class where
    put c x = c {parents = x : parents c}
instance Modifiable Class ((), Maybe Class) where
    put c ((), x) = c {cIsOf = x}
instance Modifiable Class Attribute where
    put c x = c {attributes = x : attributes c}
instance Modifiable Class Operation where
    put c x = c {operations = x : operations c}
instance Modifiable Class Slot where
    put c x = c {slots = x : slots c}

-- Attribute
instance Modifiable Attribute Level where
    put t x = t {tLevel = x}
instance Modifiable Attribute String where
    put t x = t {tName = x}
instance Modifiable Attribute Type where
    put t x = t {tType = x}
instance Modifiable Attribute Multiplicity where
    put t x = t {multiplicity = x}

-- Slot
instance Modifiable Slot Attribute where
    put t x = t {attribute = x}
instance Modifiable Slot Value where
    put t x = t {value = x}

-- Operation
instance Modifiable Operation Int where
    put o x = o {oLevel = x}
instance Modifiable Operation String where
    put o x = o {oName = x}
instance Modifiable Operation Type where
    put o x = o {oType = x}
instance Modifiable Operation Bool where
    put o x = o {isMonitored = x}
instance Modifiable Operation ((), String) where
    put o ((), x) = o {body = x}

-- Association
instance Modifiable Association String where
    put s x = s {sName = x}
instance Modifiable Association Class where
    put s x = s {sSource = x}
instance Modifiable Association ((), Class) where
    put s ((), x) = s {sTarget = x}
instance Modifiable Association Level where
    put s x = s {lvlSource = x}
instance Modifiable Association ((), Level) where
    put s ((), x) = s {lvlTarget = x}
instance Modifiable Association Multiplicity where
    put s x = s {multTargetToSource = x}
instance Modifiable Association ((), Multiplicity) where
    put s ((), x) = s {multSourceToTarget = x}
instance Modifiable Association Bool where
    put s x = s {sourceVisibleFromTarget = x}
instance Modifiable Association ((), Bool) where
    put s ((), x) = s {targetVisibleFromSource = x}

-- Link
instance Modifiable Link Association where
    put l x = l {lIsOf = x}
instance Modifiable Link Class where
    put l x = l {lSource = x}
instance Modifiable Link ((), Class) where
    put l ((), x) = l {lTarget = x}

-- Multiplicity
instance Modifiable Multiplicity Int where
    put m x = m {lower = x}
instance Modifiable Multiplicity ((), Int) where
    put m ((), x) = m {upper = x}

putValid :: (Validatable a, Modifiable a b, Show a) => a -> b -> IO a
putValid before stuff = do
    let after = put before stuff
    if valid after then do
        putStrLn "♥ still a VALID MLM after modification ♥"
        return after
    else do
        putStrLn "INVALID MLM!!!!!!!!!!!!!!!"
        putStrLn "-- before modification :"
        print before
        putStrLn "-- after  modification :"
        print after
        return before

    let
        myMlm = MLM mlmName [] [] []
        myClass1 = Class False 2 "A" [] Nothing [] [] []
        myClass2 = Class False 2 "B" [myClass1] Nothing [] [] []
    in
        insert (insert myMlm myClass1) myClass2



