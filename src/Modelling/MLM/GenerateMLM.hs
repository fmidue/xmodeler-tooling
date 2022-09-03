module Modelling.MLM.GenerateMLM (generateMLM) where

import Modelling.MLM.Types (
  MLM (..),
  Link (..),
  Association (..),
  Class (..),
  Attribute (..),
  Slot (..),
  Multiplicity (..),
  Name (..),
  Level,
  Type (..),
  Value (..),
  XModelerCurrency (..),
  emptyClass,
  emptyAssociation,
  attributeTypeSpace,
  generateScopeFinder,
  generateClassFinder,
  generateInstantiatableFinder
  )
import Modelling.MLM.Modify ((<<<), SourceOrTarget(..))
import Test.QuickCheck (elements, choose, chooseAny, chooseInt, frequency, sublistOf, Gen)
import Data.Digits (digits)
-- import Control.Monad (foldM)
import Data.Foldable (foldlM)


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
normalizeClassLevels :: [Class] -> [Class]
normalizeClassLevels classes = let lowest = minimum $ map #level classes in
    map (\x -> x <<< (#level x - lowest)) classes

randomMultGen :: (Int, Int) -> Gen Multiplicity
randomMultGen (max', chance) = do
    b <- weightedRandomXOr chance (return Nothing) (Just <$> chooseInt (1, max'))
    return $ Multiplicity (0, b)

randomSlotValue :: Attribute -> Gen Slot
randomSlotValue Attribute{name, dataType} = let
    anyFloat :: Gen Float
    anyFloat = (/100) . (fromIntegral :: Int -> Float) . round . (*100) <$> (choose (0.0,10.0) :: Gen Float)
    in do
        slotValue <- case dataType of
            Boolean -> VBoolean <$> chooseAny
            Integer -> VInteger <$> chooseInt (0,10)
            Float -> VFloat <$> anyFloat
            String -> return $ VString "some String value"
            Element -> return $ VElement "null"
            MonetaryValue -> return $ VMonetaryValue ("9.99 ","apples")
            Date -> return $ VDate (2000,01,01)
            Currency -> VCurrency <$> elements [USD, EUR, GBP, AUD, NZD]
            Complex -> return $ VComplex "null"
            AuxiliaryClass -> return $ VAuxiliaryClass "null"
        return $ Slot name slotValue


----------------------------------------------------------
-- ADDING COMPONENTS
----------------------------------------------------------

addConcretizations :: Int -> Int -> [Class] -> Gen [Class]
addConcretizations maxLvl chanceToNotConcretize theClasses = let
    concretizing :: Class -> Maybe Class -> Gen Class
    concretizing x (Just Class{name, level}) = return (x <<< Just name <<< (level - 1))
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
                    (Just <$> elements (non0Classes soFar))
                x `concretizing` toConcretize
            ) :: Gen [Class]
        return $ normalizeClassLevels torso



addInheritances :: Int -> [Class] -> Gen [Class]
addInheritances chanceToNotInherit theClasses = let

    sameAs :: Class -> Class -> Bool
    sameAs Class{level = l1, classifier = c1}
         Class {level = l2, classifier = c2} = l1 == l2 && c1 == c2
    in
    accumulate [head theClasses] (tail theClasses) (\soFar x -> do
        toInheritFrom <- weightedRandomXOr
            chanceToNotInherit
            (return [])
            (sublistOf (map #name (filter (sameAs x) soFar)))
        return $ x <<< (toInheritFrom :: [Name])
        )


addAttributes :: (Int, Int) -> [Class] -> Gen [Class]
addAttributes multSpecs theClasses = let

    addAttribute :: (Class, Int) -> Gen Class
    addAttribute (c@Class {level}, i) =
        (c <<<) <$>
        mapM (\lvl -> do
                randomType <- elements attributeTypeSpace
                randomMult <- randomMultGen multSpecs
                return $ Attribute
                    lvl
                    (attributeNameSpace !! (i + lvl))
                    randomType
                    randomMult
            ) [0..level - 1]

    classLvls :: [Level]
    classLvls = map #level theClasses

    startIndices :: [Int]
    startIndices = foldl (\soFar x -> soFar ++ [last soFar + x]) [0] classLvls

    theClasses' :: [(Class, Int)]
    theClasses' = zip theClasses startIndices

    in if null theClasses then error "Cannot add attributes. You have no classes!!!" else
        mapM addAttribute theClasses'


    -- distributedRandomlyOnto :: Int -> Int -> Gen [Int]
    -- distributedRandomlyOnto value numOfParts = do
    --     weightsInt <- vectorOf numOfParts $ chooseInt (0, precisionFactor * value) :: Gen [Int]
    --     let weights = map fromIntegral weightsInt :: [Float]
    --     let total = max 1 $ sum weights :: Float
    --     let proportions = map (/total) weights :: [Float]
    --     let result = map (round . (* fromIntegral value)) proportions :: [Int]
    --     return result

addSlotValues :: [Class] -> Gen [Class]
addSlotValues theClasses = let

    instantiatable :: Class -> [Attribute]
    instantiatable = generateInstantiatableFinder theClasses

    addSlotValue :: Class -> Gen Class
    addSlotValue c =
        (c <<<) <$> mapM randomSlotValue (instantiatable c)

    in mapM addSlotValue theClasses


addAssociations :: (Int, Int) -> Int -> [Class] -> [Association] -> Gen [Association]
addAssociations multSpecs visibilityChance theClassesIncludingLevelZero emptyAssociations = let
    theClasses = non0Classes theClassesIncludingLevelZero :: [Class]
    randomClassGen = elements theClasses :: Gen Class
    randomLevelGen x = chooseInt (0, #level x - 1) :: Gen Level
    randomMultGen' = randomMultGen multSpecs
    randomVisibilityGen = weightedRandomXOr
        visibilityChance (return True) (return False) :: Gen Bool
    in accumulateSimple emptyAssociations (\x -> do
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



-- addLinks :: [Class] -> [Association] -> Gen [Link]
-- addLinks _ _ = return []

addLinks :: [Class] -> [Association] -> Gen [Link]
addLinks theClasses theAssociations = let

    scope :: Class -> [Class]
    scope = generateScopeFinder theClasses

    getClass :: Name -> Maybe Class
    getClass = generateClassFinder theClasses

    candidateSources :: Association -> [Class]
    candidateSources Association{source, lvlSource} =
        maybe []
            (filter ((== lvlSource) . #level) . scope)
            (getClass source)

    candidateTargets :: Association -> [Class]
    candidateTargets Association{target, lvlTarget} =
        maybe []
            (filter ((== lvlTarget) . #level) . scope)
            (getClass target)

    -- participationAsSource :: Class -> Association -> Int
    -- participationAsSource Class{name} Association{source} =

    in do
        _ <- return theAssociations
        _ <- return $ scope $ head theClasses
        _ <- return $ getClass $ Name ""
        _ <- return $ candidateSources $ head theAssociations
        _ <- return $ candidateTargets $ head theAssociations
        return []



generateMLM :: String -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> Int -> Gen MLM
generateMLM projectNameString maxLvl0 numClasses0 numAssociations0 chanceToNotConcretize chanceToNotInherit multSpecsAttributes0 multSpecsAssociations0 visibilityChanceAssociations = let

    projectName = Name projectNameString :: Name

    maxLvl = max 0 maxLvl0 :: Level
    numClasses = max 1 numClasses0 :: Int
    numAssociations = max 0 numAssociations0 :: Int
    -- numAttributes = max 0 numAttributes0 :: Int
    secureMultSpecs (a, b) = (max 0 a, b)
    multSpecsAttributes = secureMultSpecs multSpecsAttributes0 :: (Int, Int)
    multSpecsAssociations = secureMultSpecs multSpecsAssociations0 :: (Int, Int)
    -- precisionFactorAttributes = max 1 precisionFactorAttributes0

    endlessEmptyClasses = map (emptyClass <<<) classNameSpace :: [Class]
    endlessEmptyAssociations = map (emptyAssociation <<<) associationNameSpace :: [Association]

    emptyClasses = take numClasses endlessEmptyClasses :: [Class]
    -- emptyAttributes = take numAttributes endlessEmptyAttributes :: [Attribute]
    emptyAssociations = take numAssociations endlessEmptyAssociations:: [Association]

    in do
        withConcretizations <- addConcretizations maxLvl chanceToNotConcretize emptyClasses
            :: Gen [Class]

        withInheritances <- addInheritances chanceToNotInherit withConcretizations
            :: Gen [Class]

        withAttributes <- addAttributes multSpecsAttributes withInheritances
            :: Gen [Class]

        withSlotValues <- addSlotValues withAttributes
            :: Gen [Class]

        readyAssociations <- addAssociations multSpecsAssociations visibilityChanceAssociations withAttributes emptyAssociations
            :: Gen [Association]

        let readyClasses = withSlotValues

        readyLinks <- addLinks readyClasses readyAssociations

        return $ MLM projectName readyClasses readyAssociations readyLinks



