module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types
import System.Random


class Modifiable a b where
    (<<<) :: a -> b -> a

data SourceOrTarget = Source | Target

-------- MLM
instance Modifiable MLM Name where
    (<<<) y x =
         y {name = x}
instance Modifiable MLM Class where
    (<<<) y@(MLM {classes}) x =
         y {classes = classes ++ [x]}
instance Modifiable MLM Association where
    (<<<) y@(MLM {associations}) x =
         y {associations = associations ++ [x]}
instance Modifiable MLM Link where
    (<<<) y@(MLM {links}) x =
         y {links = links ++ [x]}

-------- Class
instance Modifiable Class Bool where
    (<<<) y x =
         y {isAbstract = x}
instance Modifiable Class Level where
    (<<<) y x =
         y {level = x}
instance Modifiable Class Name where
    (<<<) y@(Class {parents}) x =
         y {parents = parents ++ [x]}
instance Modifiable Class (Maybe Name) where
    (<<<) y x =
         y {classifier = x}
instance Modifiable Class Attribute where
    (<<<) y@(Class {attributes}) x =
         y {attributes = attributes ++ [x]}
instance Modifiable Class Operation where
    (<<<) y@(Class {operations}) x =
         y {operations = operations ++ [x]}
instance Modifiable Class Slot where
    (<<<) y@(Class {slots}) x =
         y {slots = slots ++ [x]}

-------- Attribute
instance Modifiable Attribute Level where
    (<<<) y x =
         y {level = x}
instance Modifiable Attribute Name where
    (<<<) y x =
         y {name = x}
instance Modifiable Attribute Type where
    (<<<) y x =
         y {dataType = x}
instance Modifiable Attribute Multiplicity where
    (<<<) y x =
         y {multiplicity = x}

-------- Slot
instance Modifiable Slot Name where
    (<<<) y x =
         y {attribute = x}
instance Modifiable Slot Type where
    (<<<) y x =
         y {value = x}

-------- Operation
instance Modifiable Operation Int where
    (<<<) y x =
         y {level = x}
instance Modifiable Operation Name where
    (<<<) y x =
         y {name = x}
instance Modifiable Operation Type where
    (<<<) y x =
         y {dataType = x}
instance Modifiable Operation Bool where
    (<<<) y x =
         y {isMonitored = x}
instance Modifiable Operation OperationBody where
    (<<<) y x =
         y {body = x}

-------- Association
instance Modifiable Association (SourceOrTarget, Name) where
    (<<<) y (direction, x) =
         case direction of
            Source -> y {source = x}
            Target -> y {target = x}
instance Modifiable Association (SourceOrTarget, Level) where
    (<<<) y (direction, x) =
         case direction of
            Source -> y {lvlSource = x}
            Target -> y {lvlTarget = x}
instance Modifiable Association (SourceOrTarget, Multiplicity) where
    (<<<) y (direction, x) =
        case direction of
            Source -> y {multTargetToSource = x}
            Target -> y {multSourceToTarget = x}
instance Modifiable Association (SourceOrTarget, Bool) where
    (<<<) y (direction, x) =
        case direction of
            Source -> y {sourceVisibleFromTarget = x}
            Target -> y {targetVisibleFromSource = x}

-------- Link
instance Modifiable Link Name where
    (<<<) y x =
         y {association = x}
instance Modifiable Link (SourceOrTarget, Name) where
    (<<<) y (direction, x) =
        case direction of
            Source -> y {source = x}
            Target -> y {target = x}


getNameSpace1 :: [Char] -> [String]
getNameSpace1 chars = map (:[]) chars ++ [letter : number | number <- map show ([1..] :: [Int]), letter <- chars]

-- nameSpaceSmall :: [String]
-- nameSpaceSmall = getNameSpace1 ['a'..'z']
nameSpaceCapital :: [String]
nameSpaceCapital = getNameSpace1 ['A'..'Z']

-- getNameSpace2 :: String -> [String]
-- getNameSpace2 prefix = map ((prefix ++) . show) ([1..] :: [Int])

classNameSpace :: [Name]
classNameSpace = map Name nameSpaceCapital
-- associationNameSpace :: [Name]
-- associationNameSpace = map Name nameSpaceSmall
-- attributeNameSpace :: [Name]
-- attributeNameSpace = map Name $ getNameSpace2 "attr"
-- operationNameSpace :: [Name]
-- operationNameSpace = map Name $ getNameSpace2 "op"

-- if there is a class of level n, then there has to be at least a class of level n-1.
-- there is at least one class of level 0.

(>>>) :: Modifiable a b => b -> a -> a
(>>>) = flip (<<<)

getRandomElem :: [a] -> IO a
getRandomElem [] = error "empty list !!!"
getRandomElem list = do
    i <- randomRIO (0, length list - 1 :: Int)
    return $ list !! i

generateMLM :: Name -> Int -> Int -> IO MLM
generateMLM projectName numClasses maxLvl = let
    mlm = MLM projectName [] [] []
    classesToAdd0 = [
        Class False 0 (classNameSpace !! i) [] Nothing [] [] []
            | i <- [0..numClasses-1]]
    f soFar [] = return soFar
    f [] (x:xs) = f [x {classifier = Nothing, level = maxLvl}] xs
    f soFar (x:xs)
        | length soFar <= maxLvl =
            f (x {classifier = Just (#name (head soFar)), level = #level (head soFar) - 1} : soFar) xs
        | otherwise = do
            randElem <- getRandomElem (filter ((>0) . #level) soFar)
            f (x {classifier = Just (#name randElem), level = #level randElem - 1} : soFar) xs
    in do
        classesToAdd1 <- f [] classesToAdd0
        return $ foldr (>>>) mlm classesToAdd1


