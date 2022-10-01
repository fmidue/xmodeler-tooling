module Modelling.MLM.Generate (
    generateMLM,
    classNameSpace,
    attributeNameSpace,
    associationNameSpace,
    operationNameSpace,
    randomSlot,
    randomMult,
    randomWeightedXOr,
    ) where

import Modelling.MLM.Config (Config(..))
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
  allCurrencies,
  emptyClass,
  emptyAssociation,
  typeSpace,
  generateAboveFinder,
  generateBelowFinder,
  generateInstantiatableAttributesFinder
  )

import Modelling.MLM.Modify ((<<<), SourceOrTarget(..))
import Test.QuickCheck (elements, choose, chooseAny, chooseInt, frequency, sublistOf, shuffle, Gen, suchThat, vectorOf)
import Control.Monad (forM, foldM)
import Control.Monad.Extra (concatMapM)
import Data.Maybe (mapMaybe)
import Data.Map (Map, fromList, (!?), (!), adjust, fromListWith)
import Data.Tuple.Extra (second)
import qualified Data.Set as S (fromList)
import Data.Set (Set, member)
import Numeric (showFFloat)


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
attributeNameSpace = map Name $ getNameSpaceWithPrefix "attribute"

associationNameSpace :: [Name]
associationNameSpace = map Name $ getNameSpaceABC abcSmall

operationNameSpace :: [Name]
operationNameSpace = map Name $ getNameSpaceWithPrefix "operation"

nonLevelZero :: [Class] -> [Class]
nonLevelZero = filter ((>0) . #level)

accumulate :: [a] -> [b] -> ([a] -> b -> Gen a) -> Gen [a]
accumulate start list f = foldM (\soFar x -> do
        new <- f soFar x
        return $ soFar ++ [new]
    ) start list

randomWeightedXOr :: Float -> Gen a -> Gen a -> Gen a
randomWeightedXOr chance f g = let
        chanceF = round (chance * 1000) :: Int
        chanceG = 1000 - chanceF :: Int
    in frequency [(chanceF, f), (chanceG, g)]

randomMult :: (Float, Int) -> Gen Multiplicity
randomMult (chance, max') = do
    b <- randomWeightedXOr chance (Just <$> chooseInt (1, max')) (return Nothing)
    return $ Multiplicity (0, b)

randomSlot :: Attribute -> Gen Slot
randomSlot Attribute{name, dataType} = let
    anyFloat :: Gen Float
    anyFloat = (/100) . (fromIntegral :: Int -> Float) . round . (*100) <$> (choose (0.0,10.0) :: Gen Float)
    in do
        slotValue <- case dataType of
            Boolean -> VBoolean <$> chooseAny
            Integer -> VInteger <$> chooseInt (0,10)
            Float -> VFloat <$> anyFloat
            String -> VString <$> vectorOf 5 (elements ['a'..'f'])
            Element -> return $ VElement "null"
            MonetaryValue -> curry VMonetaryValue <$> (flip (showFFloat Nothing) "" <$> anyFloat) <*> (show <$> elements allCurrencies)
            Date -> (\year month day -> VDate (year,month,day)) <$> choose (1968,2022) <*> choose (1,12) <*> choose (1,28)
            Currency -> VCurrency <$> elements allCurrencies
            Complex -> return $ VComplex "null"
            AuxiliaryClass -> return $ VAuxiliaryClass "null"
        return $ Slot name slotValue


----------------------------------------------------------
-- ADDING COMPONENTS
----------------------------------------------------------

addConcretizations :: Level -> Float -> [Class] -> Gen [Class]
addConcretizations maxLevel chanceToConcretize theClasses = let
    concretizing :: Class -> Maybe Class -> Gen Class
    concretizing x (Just Class{name, level}) = return (x <<< Just name <<< (level - 1))
    concretizing x Nothing = do
        randomLevel <- chooseInt (1, maxLevel)
        return $ x <<< (Nothing :: Maybe Name) <<< randomLevel

    initial = head theClasses <<< maxLevel <<< (Nothing :: Maybe Name) :: Class
    vertebras = take maxLevel $ tail theClasses :: [Class]
    meat = drop (maxLevel + 1) theClasses :: [Class]

    in do
        -- if there is a class of level n, then there has to be at least a class of level n-1.
        spine <- accumulate [initial] vertebras (\soFar x -> x `concretizing` Just (last soFar))
            :: Gen [Class]
        accumulate spine meat (\soFar x -> do
                let canConcretize = nonLevelZero soFar :: [Class]
                toConcretize <- randomWeightedXOr
                    chanceToConcretize
                    (if null canConcretize then return Nothing else  Just <$> elements canConcretize)
                    (return Nothing)
                x `concretizing` toConcretize
            ) :: Gen [Class]


----------------------------------------------------------

addAbstractions :: Float -> [Class] -> Gen [Class]
addAbstractions tendency theClasses = let
    allClassifiers = S.fromList $ mapMaybe #classifier theClasses :: Set Name
    in mapM (\x@Class{name, level} -> if name `member` allClassifiers || level < 1 then return x else randomWeightedXOr tendency (return (x <<< True)) (return x)) theClasses

----------------------------------------------------------

addInheritances :: Float -> Bool -> [Class] -> Gen [Class]
addInheritances tendency allowMultipleInheritance theClasses = let

    sameAs :: Class -> Class -> Bool
    sameAs Class{level = l1, classifier = c1}
         Class {level = l2, classifier = c2} = l1 == l2 && c1 == c2

    in accumulate [head theClasses] (tail theClasses) (\soFar x -> do
            let canBeParents = map #name (filter (sameAs x) soFar)
            toInheritFrom <- if null canBeParents || #level x < 1
                then return []
                else randomWeightedXOr tendency
                    (if allowMultipleInheritance
                        then sublistOf canBeParents `suchThat` (not . null)
                        else (:[]) <$> elements canBeParents)
                    (return [])
            return $ x <<< (toInheritFrom :: [Name])
            )

----------------------------------------------------------

addAttributes :: Int -> Float -> [Class] -> Gen [Class]
addAttributes numberOfAttributesPerConcretization tendency theClasses = let

    getRandomType :: Gen Type
    getRandomType = elements typeSpace

    getName :: Int -> Name
    getName = (attributeNameSpace !!)

    thePresetMultiplicity = Multiplicity (1, Just 1) :: Multiplicity

    getRandomAttribute :: Level -> Int -> Gen Attribute
    getRandomAttribute classLevel i = do
        randomLevel <- chooseInt (0, classLevel) :: Gen Level
        let name' = getName i
        randomType <- getRandomType
        return $ Attribute randomLevel name' randomType thePresetMultiplicity

    getRandomAttributeWithSetLevel :: Level -> Int -> Gen Attribute
    getRandomAttributeWithSetLevel level' i = do
        let name' = getName i
        randomType <- getRandomType
        return $ Attribute level' name' randomType thePresetMultiplicity

    above :: Name -> [Name]
    above = map #name . generateAboveFinder theClasses Nothing

    tryToDelegateAttributeUpwards :: Name -> Gen Name
    tryToDelegateAttributeUpwards x = let
        aboveThis = above x :: [Name]
        in if null aboveThis
            then return x
            else randomWeightedXOr
                tendency
                (elements aboveThis)
                (return x)

    requestAttributesForOneClass :: ([(Name, Attribute)], Int) -> Class -> Gen ([(Name, Attribute)], Int)
    requestAttributesForOneClass (soFar, i) Class{classifier = Nothing} = return (soFar, i)
    requestAttributesForOneClass (soFar, i) Class{classifier = Just classifier', level} = do

        -- one attribute to guarantee the class will have something to instantiate
        oneAttribute <- getRandomAttributeWithSetLevel level i

        moreAttributes <- forM
            [1 .. (numberOfAttributesPerConcretization - 1)]
            (getRandomAttribute level . (i +))

        return (soFar ++ map (classifier', ) (oneAttribute : moreAttributes), i + numberOfAttributesPerConcretization)


    in do
        requestDict <- fst <$> foldM requestAttributesForOneClass ([], 0) theClasses :: Gen [(Name, Attribute)]

        requestDictWithDelegation <- mapM (\(containingClassOld, attribute) -> do
                containingClassNew <- tryToDelegateAttributeUpwards containingClassOld
                return (containingClassNew, attribute)
            ) requestDict

        let readyDict = fromListWith (++) $ map (second (:[])) requestDictWithDelegation

        return $ map (\class'@Class{name} ->
                maybe class' (class' <<<) (readyDict !? name)
            ) theClasses

----------------------------------------------------------

addSlotValues :: [Class] -> Gen [Class]
addSlotValues theClasses = let

    instantiatable :: Class -> [Attribute]
    instantiatable = generateInstantiatableAttributesFinder theClasses

    addSlotValuesForOneClass :: Class -> Gen Class
    addSlotValuesForOneClass c =
        (c <<<) <$> mapM randomSlot (instantiatable c)

    in mapM addSlotValuesForOneClass theClasses

----------------------------------------------------------

addAssociations :: (Float, Int) -> Float -> [Class] -> [Association] -> Gen [Association]
addAssociations multSpecs visibilityChance theClassesIncludingLevelZero emptyAssociations = let
    theClasses = nonLevelZero theClassesIncludingLevelZero :: [Class]
    randomClassGen = elements theClasses :: Gen Class
    randomLevelGen x = chooseInt (0, #level x - 1) :: Gen Level
    randomMult' = randomMult multSpecs
    randomVisibilityGen = randomWeightedXOr
        visibilityChance (return True) (return False) :: Gen Bool
    in forM emptyAssociations (\x -> do
                sourceClass <- randomClassGen
                targetClass <- randomClassGen
                lvlSource' <- randomLevelGen sourceClass
                lvlTarget' <- randomLevelGen targetClass
                multTargetToSource' <- randomMult'
                multSourceToTarget' <- randomMult'
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

----------------------------------------------------------

addLinks :: Float -> [Class] -> [Association] -> Gen [Link]
addLinks portionOfPossibleLinksToKeep theClasses theAssociations = let

    below :: Name -> [Class]
    below = generateBelowFinder theClasses

    getCandidateSources :: Association -> [Class]
    getCandidateSources Association{source, lvlSource} =
        filter ((== lvlSource) . #level)  .  below $
        source

    getCandidateTargets :: Association -> [Class]
    getCandidateTargets Association{target, lvlTarget} =
        filter ((== lvlTarget) . #level) . below $
        target

    addLinksForOneAssociation theAssociation@Association{name = associationName, multSource = Multiplicity (0, multSourceMaxMaybe), multTarget = Multiplicity (0, multTargetMaxMaybe)} = let

        candidateSources = map #name $ getCandidateSources theAssociation
        candidateTargets = map #name $ getCandidateTargets theAssociation

        allPossibleLinks =  [ Link associationName i j | i <- candidateSources , j <- candidateTargets]

        usedUpAsSource = maybe (const False) (==) multTargetMaxMaybe
        usedUpAsTarget = maybe (const False) (==) multSourceMaxMaybe

        in do
            let occurrencesAsSourceEmpty = fromList $ map ( , 0) candidateSources :: Map Name Int
            let occurrencesAsTargetEmpty = fromList $ map ( , 0) candidateTargets :: Map Name Int
            shuffled <- shuffle allPossibleLinks
            let (result, _ , _ ) = foldl (\(soFar, occurrencesAsSource, occurrencesAsTarget) link@Link{source = theSource, target = theTarget} ->
                        if usedUpAsSource (occurrencesAsSource ! theSource) || usedUpAsTarget (occurrencesAsTarget ! theTarget)
                            then (soFar, occurrencesAsSource, occurrencesAsTarget)
                            else (link : soFar, adjust (+1) theSource occurrencesAsSource, adjust (+1) theTarget occurrencesAsTarget)
                    ) ([], occurrencesAsSourceEmpty, occurrencesAsTargetEmpty) shuffled
            return result

    addLinksForOneAssociation _ = error "lower multiplicity bounds different from 0 not supported"

    in do
        allPossibleLinks <- shuffle =<< concatMapM addLinksForOneAssociation theAssociations :: Gen [Link]
        let numberOfLinksToKeep = round $ portionOfPossibleLinksToKeep * fromIntegral (length allPossibleLinks) :: Int
        return $ take numberOfLinksToKeep allPossibleLinks

----------------------------------------------------------

generateMLM :: Config -> Gen MLM
generateMLM Config{ projectNameString, maxClassLevel, numberOfClasses, numberOfAssociations, tendencyToConcretize, tendencyToInherit, multiplicitySpecAssociations, chanceVisibleAssociation, tendencyAbstractClass, portionOfPossibleLinksToKeep, numberOfAttributesPerConcretization, tendencyToDistanceAttributeFromItsInstantiation, allowMultipleInheritance} = let

    projectName = Name projectNameString :: Name

    endlessEmptyClasses = map (emptyClass <<<) classNameSpace :: [Class]
    endlessEmptyAssociations = map (emptyAssociation <<<) associationNameSpace :: [Association]

    emptyClasses = take numberOfClasses endlessEmptyClasses :: [Class]
    emptyAssociations = take numberOfAssociations endlessEmptyAssociations :: [Association]

    in do
        withConcretizations <- addConcretizations maxClassLevel tendencyToConcretize emptyClasses :: Gen [Class]
        withAbstractions <- addAbstractions tendencyAbstractClass withConcretizations :: Gen [Class]
        withInheritances <- addInheritances tendencyToInherit allowMultipleInheritance withAbstractions :: Gen [Class]
        withAttributes <- addAttributes numberOfAttributesPerConcretization tendencyToDistanceAttributeFromItsInstantiation withInheritances :: Gen [Class]
        readyClasses <- addSlotValues withAttributes :: Gen [Class]

        readyAssociations <- addAssociations multiplicitySpecAssociations chanceVisibleAssociation withAttributes emptyAssociations :: Gen [Association]

        readyLinks <- addLinks portionOfPossibleLinksToKeep readyClasses readyAssociations

        return $ MLM projectName readyClasses readyAssociations readyLinks
