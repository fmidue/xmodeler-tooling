module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types
import Test.QuickCheck (elements, generate, chooseInt, frequency)
import Data.Digits (digits)

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
-- -------- let y concretize x
-- instance Modifiable Class Class where
--     (<<<) y (Class {name, level}) =
--         y <<< Just name <<< (level - 1)
-- -------- let y inherit from xs
-- instance Modifiable Class [Class] where
--     (<<<) y xs =
--         y {parents = map #name xs}

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

-- (>>>) :: Modifiable a b => b -> a -> a
-- (>>>) = flip (<<<)

-- -------- let y concretize x
--  Modifiable Class Class where
--     (<<<) y (Class {name, level}) =
--         y <<< Just name <<< (level - 1)
-- -------- let y inherit from xs
-- instance Modifiable Class [Class] where
--     (<<<) y xs =
--         y {parents = map #name xs}

normalizeClassLevels :: [Class] -> [Class]
normalizeClassLevels classes = let lowest = minimum $ map #level classes in
    map (\x -> x <<< (#level x - lowest)) classes

generateMLM :: Name -> Int -> Int -> Int -> Int -> IO MLM
generateMLM projectName numClasses0 maxLvl0 chanceToNotConcretize chanceToNotInherit = let
    -- addConcretizations :: [Class] -> Level -> [Class] -> IO [Class]
    -- addConcretizations [] _ soFar =
    --     return soFar
    -- addConcretizations (x:xs) level [] =
    --     addConcretizations xs (level - 1) [x <<< level <<< (Nothing :: Maybe Name)]
    -- addConcretizations (x:xs) 0 soFar = do
    --     randomClass <- generate $ elements $ filter ((>0) . #level) soFar
    --     x' <- x `concretizing` Just randomClass
    --     addConcretizations xs 0 (x' : soFar)
    -- addConcretizations (x:xs) level soFar = do
    --     x' <- x `concretizing` Just (last soFar)
    --     addConcretizations xs (level - 1) (soFar ++ [x'])

    numClasses = max 1 numClasses0
    maxLvl = max 0 maxLvl0
    classesToAdd0 = [Class False 0 (classNameSpace !! i) [] Nothing [] [] [] | i <- [0..numClasses-1]] :: [Class]

    percentize :: Int -> Int
    percentize = (10 ^) . length . digits 10 . abs
    totalWeightConcretization = percentize chanceToNotConcretize
    -- totalWeightInheritance = percentize chanceToNotInherit

    concretizing :: Class -> Maybe Class -> IO Class
    concretizing x (Just (Class {name, level})) = return $ x <<< Just name <<< (level - 1)
    concretizing x Nothing = do
        randomLevel <- generate $ chooseInt (1, max 1 maxLvl)
        return $ x <<< (Nothing :: Maybe Name) <<< randomLevel

    testShowClass x = (#name x, #level x, #classifier x)
    testShowClass1 = map testShowClass

    in do
        print chanceToNotInherit

        let a = return [head classesToAdd0 <<< maxLvl <<< (Nothing :: Maybe Name)]
        putStrLn "a = "
        a1 <- a
        print $ testShowClass1 a1
        let b = take maxLvl $ tail classesToAdd0
        putStrLn "b = "
        print $ testShowClass1 b
        let c = drop (maxLvl + 1) classesToAdd0
        putStrLn "c = "
        print $ testShowClass1 c
        let d = foldl (\soFarIO x -> do
                        soFar <- soFarIO
                        newClass <- x `concretizing` Just (last soFar)
                        return $ soFar ++ [newClass]
                        ) a b
        putStrLn "d = "
        d1 <- d
        print $ testShowClass1 d1
        let e = foldr (\x soFarIO -> do
                        soFar <- soFarIO
                        newClass <- generate (frequency [
                            (chanceToNotConcretize,
                                return (x `concretizing` Nothing)),
                            (totalWeightConcretization - chanceToNotConcretize ,
                                do
                                    randomClass <- elements (filter ((>0) . #level) soFar)
                                    return (x `concretizing` Just randomClass)
                                )
                            ])
                        newClass' <- newClass
                        return $ soFar ++ [newClass']
                       ) d c
        putStrLn "e = "
        e1 <- e
        print $ testShowClass1 e1
        classesToAdd <- e
        let classesToAdd' = normalizeClassLevels classesToAdd

        return $ MLM projectName classesToAdd' [] []



