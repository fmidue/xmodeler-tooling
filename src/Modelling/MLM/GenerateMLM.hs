module Modelling.MLM.GenerateMLM (generateMLM, (<<<)) where

import Modelling.MLM.Types (
  MLM (..),
  Link (..),
  Association (..),
  Slot (..),
  Class (..),
  Operation (..),
  Attribute (..),
  Multiplicity (..),
  Name (..),
  OperationBody (..),
  Level,
  Type (..)
  )
import Test.QuickCheck (elements, chooseInt, frequency, sublistOf, vectorOf, Gen)
import Data.Digits (digits)
import Data.Foldable (foldlM)

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

abcCapital :: [String]
abcCapital = map (:[]) ['A'..'Z']

abcSmall :: [String]
abcSmall = map (:[]) ['a'..'z']

getNameSpaceWithPrefix :: String -> [String]
getNameSpaceWithPrefix prefix = map ((prefix ++) . show) ([1..] :: [Int])

getNameSpaceABC :: [String] -> [String]
getNameSpaceABC abc = abc ++ [i ++ j | i <- getNameSpaceABC abc, j <- abc]

classNameSpace :: [Name]
classNameSpace = map Name $ getNameSpaceABC abcCapital

attributeNameSpace :: [Name]
attributeNameSpace = map Name $ getNameSpaceWithPrefix "attr"

associationNameSpace :: [Name]
associationNameSpace = map Name $ getNameSpaceABC abcSmall

-- operationNameSpace :: [Name]
-- operationNameSpace = map Name $ getNameSpaceWithPrefix "op"

distributedRandomlyOnto :: Int -> Int -> Gen [Int]
distributedRandomlyOnto value numOfParts = do
    weightsInt <- vectorOf numOfParts $ chooseInt (0, precisionFactor * value) :: Gen [Int]
    let weights = map fromIntegral weightsInt :: [Float]
    let total = max 1 $ sum weights :: Float
    let proportions = map (/total) weights :: [Float]
    let result = map (round . (* fromIntegral value)) proportions :: [Int]
    return result

non0Classes :: [Class] -> [Class]
non0Classes = filter ((>0) . #level)

accumulate :: [a] -> [b] -> ([a] -> b -> Gen a) -> Gen [a]
accumulate start list f = foldlM (\soFar x -> do
        new <- f soFar x
        return $ soFar ++ [new]
    ) start list

accumulateSimple :: [b] -> (b -> Gen a) -> Gen [a]
accumulateSimple list f = foldlM (\soFar x -> do
        new <- f x
        return $ soFar ++ [new]
    ) [] list

weightedRandomXOr :: Int -> Gen a -> Gen a -> Gen a
weightedRandomXOr chance f g =
    frequency [(chance, f), (complement chance, g)]

complement :: Int -> Int
complement x = ((10 ^) . length . digits 10 . abs) x - x

-- there must be at least one class of level 0.
normalizeClassLevels :: [Class] -> [Class]
normalizeClassLevels classes = let lowest = minimum $ map #level classes in
    map (\x -> x <<< (#level x - lowest)) classes

----------------------------------------------------------
-- ADDING COMPONENTS
----------------------------------------------------------

addConcretizations :: Int -> Int -> [Class] -> Gen [Class]
addConcretizations maxLvl chanceToNotConcretize theClasses = let
    concretizing :: Class -> Maybe Class -> Gen Class
    concretizing x (Just (Class {name, level})) = return $ x <<< Just name <<< (level - 1)
    concretizing x Nothing = do
        randomLevel <- chooseInt (1, max 1 maxLvl)
        -- max 1 ... because having meta classes of level zero is useless
        return $ x <<< (Nothing :: Maybe Name) <<< randomLevel

    initial = head theClasses <<< maxLvl <<< (Nothing :: Maybe Name) :: Class
    vertebras = take maxLvl $ tail theClasses :: [Class]
    meat = drop (maxLvl + 1) theClasses :: [Class]

    in do
        -- if there is a class of level n, then there has to be at least a class of level n-1.
        spine <- accumulate [initial] vertebras (\soFar x -> x `concretizing` Just (last soFar))
            :: Gen [Class]
        torso <- accumulate spine meat (\soFar x -> do
                toConcretize <- weightedRandomXOr
                    chanceToNotConcretize
                    (return Nothing)
                    (do randomClass <- elements (non0Classes soFar)
                        return (Just randomClass))
                x `concretizing` toConcretize
            ) :: Gen [Class]
        return $ normalizeClassLevels torso



addInheritances :: Int -> [Class] -> Gen [Class]
addInheritances chanceToNotInherit theClasses = let
    in accumulate [head theClasses] (tail theClasses) (\soFar x -> do
            toInheritFrom <- weightedRandomXOr
                chanceToNotInherit
                (return [])
                (sublistOf (map #name (filter ((== #level x) . #level) soFar)))
            return $ x <<< (toInheritFrom :: [Name])
            )



addAttributes :: Int -> [Attribute] -> [Class] -> Gen [Class]
addAttributes numAttributes theAttributes theClasses = let
    numNon0Classes = length $ non0Classes theClasses :: Int
    in do
        numOfAttributesForEachClass0 <- numAttributes `distributedRandomlyOnto` numNon0Classes
            :: Gen [Int]

        let numOfAttributesForEachClass =
                snd $ foldl (\(numAttrs0, soFar) class' ->
                    if #level class' == 0
                        then (numAttrs0, soFar ++ [0])
                        else (tail numAttrs0, soFar ++ [head numAttrs0]))
                    (numOfAttributesForEachClass0 :: [Int], [] :: [Int])
                    theClasses
                :: [Int]

        if numNon0Classes == length numOfAttributesForEachClass0
            then return ()
            else error "ERROR : Number of attributes portions is different from number of classes!!!"

        let attributesToAdd =
                snd $ foldl (\(attributesRemaining, soFar) n ->
                    (drop n attributesRemaining, soFar ++ [take n attributesRemaining]))
                    (theAttributes, [] :: [[Attribute]])
                    numOfAttributesForEachClass
                :: [[Attribute]]

        let withLevelZeroAttributes = zipWith (<<<) theClasses attributesToAdd
                :: [Class]

        accumulateSimple withLevelZeroAttributes (\x -> do
                toAdd <- accumulateSimple (#attributes x) (\attr -> do
                        randomLevel <- chooseInt (0, #level x - 1)
                        return $ attr <<< randomLevel
                    )
                return $ x <<< toAdd
            ) :: Gen [Class]



addAssociations :: Int -> Int -> [Class] -> [Association] -> Gen [Association]
addAssociations maxMultAssociations visibilityChanceAssociations theClassesIncludingLevelZero emptyAssociations = let
    theClasses = non0Classes theClassesIncludingLevelZero
    randomClassGen = elements theClasses
    randomLevelGen x = chooseInt (0, #level x - 1)
    randomMultGen = elements [(i,j) | i <- [0..maxMultAssociations], j <- (-1):[i..maxMultAssociations]]
    randomVisibilityGen = weightedRandomXOr
        visibilityChanceAssociations (return True) (return False)
    in do
        accumulateSimple emptyAssociations (\x -> do
                sourceClass <- randomClassGen
                targetClass <- randomClassGen
                lvlSource' <- randomLevelGen sourceClass
                lvlTarget' <- randomLevelGen targetClass
                multTargetToSource' <- randomMultGen
                multSourceToTarget' <- randomMultGen
                sourceVisibleFromTarget' <- randomVisibilityGen
                targetVisibleFromSource' <- randomVisibilityGen
                return $ x
                    <<< (Source, #name sourceClass)
                    <<< (Target, #name targetClass)
                    <<< (Source, lvlSource')
                    <<< (Target, lvlTarget')
                    <<< (Source, Multiplicity multTargetToSource')
                    <<< (Target, Multiplicity  multSourceToTarget')
                    <<< (Source, sourceVisibleFromTarget') <<< (Target, targetVisibleFromSource')
            )



precisionFactor :: Int
precisionFactor = 1000

generateMLM :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Gen MLM
generateMLM projectNameString maxLvl0 numClasses0 numAssociations0 chanceToNotConcretize chanceToNotInherit numAttributes0 maxMultAssociations visibilityChanceAssociations= let

    projectName = Name projectNameString :: Name

    maxLvl = max 0 maxLvl0 :: Level
    numClasses = max 1 numClasses0 :: Int
    numAssociations = max 0 numAssociations0 :: Int
    numAttributes = max 0 numAttributes0 :: Int

    emptyClasses = [Class False 0 (classNameSpace !! i) [] Nothing [] [] []
        | i <- [0..numClasses-1]] :: [Class]
    emptyAttributes = [Attribute 0 (attributeNameSpace !! i) (Boolean Nothing) (Multiplicity (1,1))
        | i <- [0..numAttributes-1]] :: [Attribute]
    emptyAssociations = [Association (associationNameSpace !! i) (Name "") (Name "") 0 0 (Multiplicity (0,1)) (Multiplicity (0,1)) True True
        | i <- [0..numAssociations-1]] :: [Association]

    in do
        withConcretizations <- addConcretizations maxLvl chanceToNotConcretize emptyClasses
            :: Gen [Class]

        withInheritances <- addInheritances chanceToNotInherit withConcretizations
            :: Gen [Class]

        withAttributes <- addAttributes numAttributes emptyAttributes withInheritances
            :: Gen [Class]

        readyAssociations <- addAssociations maxMultAssociations visibilityChanceAssociations withAttributes emptyAssociations
            :: Gen [Association]

        let readyClasses = withAttributes
        return $ MLM projectName readyClasses readyAssociations []



