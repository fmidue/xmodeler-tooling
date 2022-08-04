module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types
import Test.QuickCheck (elements, generate, chooseInt, frequency, sublistOf)
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
instance Modifiable Class [Name] where
    (<<<) y x =
         y {parents = x}
instance Modifiable Class (Maybe Name) where
    (<<<) y x =
         y {classifier = x}
instance Modifiable Class [Attribute] where
    (<<<) y x =
         y {attributes = x}
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


-- getNameSpace1 :: [Char] -> [String]
-- getNameSpace1 chars = map (:[]) chars ++ [letter : number | number <- map show ([1..] :: [Int]), letter <- chars]

-- nameSpaceSmall :: [String]
-- nameSpaceSmall = getNameSpace1 ['a'..'z']
-- nameSpaceCapital :: [String]
-- nameSpaceCapital = getNameSpace1 ['A'..'Z']

getNameSpace2 :: String -> [String]
getNameSpace2 prefix = map ((prefix ++) . show) ([1..] :: [Int])

abc :: [String]
abc = map (:[]) ['A'..'Z']

classNameSpace0 :: [String]
classNameSpace0 = abc ++ [j ++ i | j <- classNameSpace0, i <- abc ]

classNameSpace :: [Name]
classNameSpace = map Name classNameSpace0

-- associationNameSpace :: [Name]
-- associationNameSpace = map Name nameSpaceSmall
attributeNameSpace :: [Name]
attributeNameSpace = map Name $ getNameSpace2 "attr"
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

testShowClass :: Class -> (Name, Level, Maybe Name)
testShowClass x = (#name x, #level x, #classifier x)
testShowClass1 :: [Class] -> [(Name, Level, Maybe Name)]
testShowClass1 = map testShowClass

normalizeClassLevels :: [Class] -> [Class]
normalizeClassLevels classes = let lowest = minimum $ map #level classes in
    map (\x -> x <<< (#level x - lowest)) classes

percentize :: Int -> Int
percentize = (10 ^) . length . digits 10 . abs

distributeRandomlyOnto :: Int -> Int -> IO [Int]
distributeRandomlyOnto value numOfParts = let
    generateWeights :: Int -> [Int] -> IO [Int]
    generateWeights 0 soFar = return soFar
    generateWeights n soFar = do
        x <- generate $ chooseInt (0,100)
        generateWeights (n-1) (x : soFar)
    in do
        weights <- generateWeights numOfParts []
        let total = fromIntegral (sum weights) :: Float
        let proportions = map ((/total) . fromIntegral) weights :: [Float]
        return (map (round . (* fromIntegral value)) proportions)

non0Classes :: [Class] -> [Class]
non0Classes = filter ((>0) . #level)

generateMLM :: String -> Int -> Int -> Int -> Int -> Int -> Int -> IO MLM
generateMLM projectNameString maxLvl0 numClasses0 numAssociations0 chanceToNotConcretize chanceToNotInherit numAttributes0 = let

    projectName = Name projectNameString

    maxLvl = max 0 maxLvl0
    numClasses = max 1 numClasses0
    numAssociations = max 0 numAssociations0
    numAttributes = max 0 numAttributes0

    classesToAdd0 = [Class False 0 (classNameSpace !! i) [] Nothing [] [] [] | i <- [0..numClasses-1]] :: [Class]
    attributesToAdd0 = [Attribute 0 (attributeNameSpace !! i) (Boolean Nothing) (Multiplicity (1,1)) | i <- [0..numAttributes-1]]

    chanceToConcretize = percentize chanceToNotConcretize - chanceToNotConcretize
    chanceToInherit = percentize chanceToNotInherit - chanceToNotInherit

    concretizing :: Class -> Maybe Class -> IO Class
    concretizing x (Just (Class {name, level})) = return $ x <<< Just name <<< (level - 1)
    concretizing x Nothing = do
        randomLevel <- generate $ chooseInt (1, max 1 maxLvl)
        -- max 1 ... because having meta classes of level zero is useless
        return $ x <<< (Nothing :: Maybe Name) <<< randomLevel

    in do
        -- Concretizations:
        let initial = head classesToAdd0 <<< maxLvl <<< (Nothing :: Maybe Name)
        let vertebras = take maxLvl $ tail classesToAdd0
        let meat = drop (maxLvl + 1) classesToAdd0
        let spine = foldl (\soFarIO x -> do
                        soFar <- soFarIO
                        newClass <- x `concretizing` Just (last soFar)
                        return $ soFar ++ [newClass]
                        ) (return [initial]) vertebras
        let torso = foldr (\x soFarIO -> do
                        soFar <- soFarIO
                        newClass <- generate (frequency [
                            (chanceToNotConcretize,
                                return (x `concretizing` Nothing)),
                            (chanceToConcretize ,
                                do
                                    randomClass <- elements (filter ((>0) . #level) soFar)
                                    return (x `concretizing` Just randomClass)
                                )
                            ])
                        newClass' <- newClass
                        return $ soFar ++ [newClass']
                       ) spine meat
        torso' <- torso
        let normalized = normalizeClassLevels torso'

        -- Inheritances:
        let withInheritances = foldl (\soFarIO x -> do
                        soFar <- soFarIO
                        x' <- generate (frequency [
                            (chanceToNotInherit,
                                return x),
                            (chanceToInherit,
                                do
                                    randomSubset <- sublistOf
                                        $ map #name $ filter ((== #level x) . #level) soFar
                                    return (x <<< randomSubset))
                            ])
                        return $ soFar ++ [x']
                        ) (return [head normalized]) (tail normalized)

        -- Attributes:
        withoutAttributes <- withInheritances
        let numNon0Classes = length $ non0Classes withoutAttributes
        numOfAttributesForEachClass0 <- numAttributes `distributeRandomlyOnto` numNon0Classes :: IO [Int]
        if numNon0Classes == length numOfAttributesForEachClass0
            then putStrLn "Attributes assigned!"
            else error "ERROR : Number of attributes portions is different from number of classes!!!"

        let numOfAttributesForEachClass = snd $ foldl (\(numAttrs0, soFar) class' -> if #level class' == 0 then (numAttrs0, soFar ++ [0]) else (tail numAttrs0, soFar ++ [head numAttrs0])) (numOfAttributesForEachClass0 :: [Int], [] :: [Int]) withoutAttributes :: [Int]
        let attributesToAdd = snd $ foldl (\(attributesRemaining, soFar) n -> (drop n attributesRemaining, soFar ++ [take n attributesRemaining])) (attributesToAdd0, [] :: [[Attribute]]) numOfAttributesForEachClass
        let withAttributesButAllLevel0 = zipWith (<<<) withoutAttributes attributesToAdd
        let withAttributes = foldl (\soFarIO class' -> do
                                        soFar <- soFarIO
                                        class'' <- foldl (\x attr -> do
                                                          classCurrently <- x
                                                          randomLevel <- generate $ chooseInt (0, #level classCurrently - 1)
                                                          let attr' = attr <<< randomLevel
                                                          return $ classCurrently <<< (tail (#attributes classCurrently) ++ [attr'])
                                                         ) (return class') (#attributes class')
                                        return $ soFar ++ [class'']
                                    ) (return []) withAttributesButAllLevel0

        -- Slots:

        -- Associations:
        print numAssociations

        classesToAdd <- withAttributes

        putStrLn "initial = "
        print initial
        putStrLn "spine = "
        print $ testShowClass1 vertebras
        putStrLn "meat = "
        print $ testShowClass1 meat
        putStrLn "torso = "
        d1 <- torso
        print $ testShowClass1 d1
        putStrLn "normalized = "
        print $ testShowClass1 normalized
        return $ MLM projectName classesToAdd [] []



