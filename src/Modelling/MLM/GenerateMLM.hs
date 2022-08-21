module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types (
  MLM (..),
  Link (..),
  Association (..),
  Class (..),
  Attribute (..),
  Multiplicity (..),
  Name (..),
  Level,
  Type (..)
  )
import Modelling.MLM.Modify ((<<<), SourceOrTarget(..))
import Test.QuickCheck (elements, chooseInt, frequency, sublistOf, vectorOf, Gen)
import Data.Digits (digits)
import Data.Foldable (foldlM)
import Data.Tuple.Extra (first)
import Control.Applicative ((<|>))

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
normalizeClassLevels :: [(Class, Maybe Name)] -> [(Class, Maybe Name)]
normalizeClassLevels classes = let lowest = minimum $ map (#level . fst) classes in
    map (first (\x -> x <<< (#level x - lowest))) classes

randomMultGen :: (Int, Int) -> Gen Multiplicity
randomMultGen (max', chance) = do
    b <- weightedRandomXOr chance (return Nothing) (Just <$> chooseInt (1, max'))
    return $ Multiplicity (0, b)

----------------------------------------------------------
-- ADDING COMPONENTS
----------------------------------------------------------

addConcretizations :: Int -> Int -> [Class] -> Gen [(Class, Maybe Name)]
addConcretizations maxLvl chanceToNotConcretize theClasses = let
    concretizing :: Class -> (Maybe Class, Maybe Name) -> Gen (Class, Maybe Name)
    concretizing x (Just (Class {name, level}), metaClass) = return (x <<< Just name <<< (level - 1), metaClass <|> Just name)
    concretizing x (Nothing, _) = do
        randomLevel <- chooseInt (1, max 1 maxLvl)
        -- max 1 ... because having meta classes of level zero is useless
        return (x <<< (Nothing :: Maybe Name) <<< randomLevel, Nothing)

    initial = (head theClasses <<< maxLvl <<< (Nothing :: Maybe Name), Nothing) :: (Class, Maybe Name)
    vertebras = take maxLvl $ tail theClasses :: [Class]
    meat = drop (maxLvl + 1) theClasses :: [Class]

    in do
        -- if there is a class of level n, then there has to be at least a class of level n-1.
        spine <- accumulate [initial] vertebras (\soFar x -> x `concretizing` first Just (last soFar))
            :: Gen [(Class, Maybe Name)]
        torso <- accumulate spine meat (\soFar x -> do
                toConcretize <- weightedRandomXOr
                    chanceToNotConcretize
                    (return (Nothing, Nothing))
                    (do randomClass <- elements (filter ((>0) . #level . fst) soFar)
                        return (first Just randomClass))
                x `concretizing` toConcretize
            ) :: Gen [(Class, Maybe Name)]
        return $ normalizeClassLevels torso



addInheritances :: Int -> [(Class, Maybe Name)] -> Gen [Class]
addInheritances chanceToNotInherit theClasses = map fst <$> accumulate [head theClasses] (tail theClasses) (\soFar (theClass, theMetaClass) -> do
            toInheritFrom <- weightedRandomXOr
                chanceToNotInherit
                (return [])
                (sublistOf (map (#name . fst) (filter (\(Class {level = itsLevel}, itsMetaClass) -> itsLevel == #level theClass && itsMetaClass == theMetaClass) soFar)))
            return (theClass <<< (toInheritFrom :: [Name]), theMetaClass)
            )


addAttributes :: (Int, Int) -> Int -> Int -> [Attribute] -> [Class] -> Gen [Class]
addAttributes multSpecs precisionFactor numAttributes theAttributes theClasses = let
    distributedRandomlyOnto :: Int -> Int -> Gen [Int]
    distributedRandomlyOnto value numOfParts = do
        weightsInt <- vectorOf numOfParts $ chooseInt (0, precisionFactor * value) :: Gen [Int]
        let weights = map fromIntegral weightsInt :: [Float]
        let total = max 1 $ sum weights :: Float
        let proportions = map (/total) weights :: [Float]
        let result = map (round . (* fromIntegral value)) proportions :: [Int]
        return result

    numNon0Classes = length $ non0Classes theClasses :: Int
    randomMultGen' = randomMultGen multSpecs
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
                        randomMult <- randomMultGen'
                        randomType <- elements [
                            Boolean Nothing,
                            Integer Nothing,
                            Float Nothing,
                            String Nothing,
                            Element Nothing,
                            MonetaryValue Nothing,
                            Date Nothing,
                            Currency Nothing,
                            Complex Nothing,
                            AuxiliaryClass Nothing
                            ]
                        return $ attr <<< randomLevel <<< randomMult <<< randomType
                    )
                return $ x <<< toAdd
            ) :: Gen [Class]



addAssociations :: (Int, Int) -> Int -> [Class] -> [Association] -> Gen [Association]
addAssociations multSpecs visibilityChance theClassesIncludingLevelZero emptyAssociations = let
    theClasses = non0Classes theClassesIncludingLevelZero :: [Class]
    randomClassGen = elements theClasses :: Gen Class
    randomLevelGen x = chooseInt (0, #level x - 1) :: Gen Level
    randomMultGen' = randomMultGen multSpecs
    randomVisibilityGen = weightedRandomXOr
        visibilityChance (return True) (return False) :: Gen Bool
    in do
        accumulateSimple emptyAssociations (\x -> do
                sourceClass <- randomClassGen
                targetClass <- randomClassGen
                lvlSource' <- randomLevelGen sourceClass
                lvlTarget' <- randomLevelGen targetClass
                multTargetToSource' <- randomMultGen'
                multSourceToTarget' <- randomMultGen'
                sourceVisibleFromTarget' <- randomVisibilityGen
                targetVisibleFromSource' <- randomVisibilityGen
                return $ x
                    <<< (Source, #name sourceClass)
                    <<< (Target, #name targetClass)
                    <<< (Source, lvlSource')
                    <<< (Target, lvlTarget')
                    <<< (Source, multTargetToSource')
                    <<< (Target, multSourceToTarget')
                    <<< (Source, sourceVisibleFromTarget')
                    <<< (Target, targetVisibleFromSource')
            )



addLinks :: [Class] -> [Association] -> Gen [Link]
addLinks _ _ = return []






generateMLM :: String -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> Int -> (Int, Int) -> Int -> Gen MLM
generateMLM projectNameString maxLvl0 numClasses0 numAssociations0 chanceToNotConcretize chanceToNotInherit
    numAttributes0 multSpecsAttributes0 precisionFactorAttributes0 multSpecsAssociations0 visibilityChanceAssociations = let

    projectName = Name projectNameString :: Name

    maxLvl = max 0 maxLvl0 :: Level
    numClasses = max 1 numClasses0 :: Int
    numAssociations = max 0 numAssociations0 :: Int
    numAttributes = max 0 numAttributes0 :: Int
    secureMultSpecs (a, b) = (max 0 a, b)
    multSpecsAttributes = secureMultSpecs multSpecsAttributes0 :: (Int, Int)
    multSpecsAssociations = secureMultSpecs multSpecsAssociations0 :: (Int, Int)
    precisionFactorAttributes = max 1 precisionFactorAttributes0

    emptyClasses = [Class False 0 (classNameSpace !! i) [] Nothing [] [] []
        | i <- [0..numClasses-1]] :: [Class]
    -- emptyClasses = zipWith (<<<) (replicate numClasses emptyClass)  ???

    emptyAttributes = [Attribute 0 (attributeNameSpace !! i) (Boolean Nothing) (Multiplicity (1,Nothing))
        | i <- [0..numAttributes-1]] :: [Attribute]
    emptyAssociations = [Association (associationNameSpace !! i) (Name "") (Name "") 0 0 (Multiplicity (0,Nothing)) (Multiplicity (0,Nothing)) True True
        | i <- [0..numAssociations-1]] :: [Association]

    in do
        withConcretizationsAndMetaClassesDict <- addConcretizations maxLvl chanceToNotConcretize emptyClasses
            :: Gen [(Class, Maybe Name)]

        withInheritances <- addInheritances chanceToNotInherit withConcretizationsAndMetaClassesDict
            :: Gen [Class]

        withAttributes <- addAttributes multSpecsAttributes precisionFactorAttributes numAttributes emptyAttributes withInheritances
            :: Gen [Class]

        readyAssociations <- addAssociations multSpecsAssociations visibilityChanceAssociations withAttributes emptyAssociations
            :: Gen [Association]

        let readyClasses = withAttributes

        readyLinks <- addLinks readyClasses readyAssociations

        return $ MLM projectName readyClasses readyAssociations readyLinks



