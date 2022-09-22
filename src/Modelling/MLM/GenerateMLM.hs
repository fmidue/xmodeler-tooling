module Modelling.MLM.GenerateMLM (
    generateMLM,
    classNameSpace,
    attributeNameSpace,
    associationNameSpace,
    operationNameSpace,
    randomSlot,
    randomMult,
    randomWeightedXOr,
    addSlotValues
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
  XModelerCurrency (..),
  emptyClass,
  emptyAssociation,
  typeSpace,
  generateBelowFinder,
  generateClassFinder,
  generateInstantiatableAttributesFinder
  )

import Modelling.MLM.Modify ((<<<), SourceOrTarget(..))
import Test.QuickCheck (elements, choose, chooseAny, chooseInt, frequency, sublistOf, shuffle, Gen)
import Control.Monad (forM, foldM)
import Data.Maybe (fromMaybe)
import Data.Map (Map, fromList, insertWith, (!?), (!), adjust)



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
randomWeightedXOr chance f g = if chance < 0 || chance > 1
    then error "Chance values must be in the inclusive range (0, 1) !!!"
    else let
        chanceTo = round (chance * 1000) :: Int
        chanceNotTo = 1000 - chanceTo :: Int
    in
        frequency [(chanceTo, f), (chanceNotTo, g)]

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

addConcretizations :: Level -> Float -> [Class] -> Gen [Class]
addConcretizations maxLevel chanceToConcretize theClasses = let
    concretizing :: Class -> Maybe Class -> Gen Class
    concretizing x (Just Class{name, level}) = return (x <<< Just name <<< (level - 1))
    concretizing x Nothing = do
        randomLevel <- chooseInt (1, max 1 maxLevel)
        -- max 1 ... because having meta classes of level zero is useless
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
    allClassifiers = map #classifier theClasses :: [Maybe Name]
    in mapM (\x -> if Just (#name x) `elem` allClassifiers || #level x < 1 then return x else (x <<<) <$> randomWeightedXOr tendency (return False) (return True)) theClasses

----------------------------------------------------------

addInheritances :: Float -> [Class] -> Gen [Class]
addInheritances tendency theClasses = let

    sameAs :: Class -> Class -> Bool
    sameAs Class{level = l1, classifier = c1}
         Class {level = l2, classifier = c2} = l1 == l2 && c1 == c2

    in accumulate [head theClasses] (tail theClasses) (\soFar x -> do
            toInheritFrom <- randomWeightedXOr
                tendency
                (if #level x > 0 then sublistOf (map #name (filter (sameAs x) soFar)) else return [])
                (return [])
            return $ x <<< (toInheritFrom :: [Name])
            )

----------------------------------------------------------

addAttributes :: Int -> Float -> [Class] -> Gen [Class]
addAttributes averageNumberOfAttributesPerClass tendency theClasses = let

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

    getRandomAttributeSetLevel :: Level -> Int -> Gen Attribute
    getRandomAttributeSetLevel level' i = do
        let name' = getName i
        randomType <- getRandomType
        return $ Attribute level' name' randomType thePresetMultiplicity

    findClass :: Name -> Maybe Class
    findClass = generateClassFinder theClasses

    tryToDelegateAttributeToClassifier :: Name -> Gen Name
    tryToDelegateAttributeToClassifier x =
        randomWeightedXOr
            tendency
            (return (maybe x (\Class{classifier} -> fromMaybe x classifier) (findClass x)))
            (return x)

    addAttributesForOneClass :: (Map Name [Attribute], Int) -> Class -> Gen (Map Name [Attribute], Int)
    addAttributesForOneClass (soFar, i) Class{classifier = Nothing} = return (soFar, i)
    addAttributesForOneClass (soFar, i) Class{classifier = Just classifier', level} = do

        -- one attribute to guarantee the class will have something to instantiate
        oneAttribute <- getRandomAttributeSetLevel level i

        moreAttributes <- forM
            [1 .. (averageNumberOfAttributesPerClass - 1)]
            (getRandomAttribute level . (i + ))

        containingClass <- tryToDelegateAttributeToClassifier classifier'

        return (insertWith (++) containingClass (oneAttribute : moreAttributes) soFar
                , i + max 1 averageNumberOfAttributesPerClass)


    in do
        let emptyRequestMap = fromList $ map (( , []) . #name) theClasses :: Map Name [Attribute]

        requestDict <- fst <$> foldM addAttributesForOneClass (emptyRequestMap, 0) theClasses :: Gen (Map Name [Attribute])

        return $ map (\class'@Class{name} ->
                maybe class' (class' <<<) (requestDict !? name)
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

    below :: Class -> [Class]
    below = generateBelowFinder theClasses

    getClass :: Name -> Maybe Class
    getClass = generateClassFinder theClasses

    getCandidateSources :: Association -> [Class]
    getCandidateSources Association{source, lvlSource} = maybe
        (error "An association is referring to a non-existent class as its source!!!")
        (filter ((== lvlSource) . #level)  .  below)
        (getClass source)

    getCandidateTargets :: Association -> [Class]
    getCandidateTargets Association{target, lvlTarget} = maybe
        (error "An association is referring to a non-existent class as its target!!!")
        (filter ((== lvlTarget) . #level) . below)
        (getClass target)

    inverted :: Link -> Link
    inverted Link{name, source, target} = Link name target source

    addLinksForOneAssociation theAssociation@Association{name = associationName, multSource = Multiplicity (_, multSourceMaxMaybe), multTarget = Multiplicity (_, multTargetMaxMaybe)} = let

        candidateSources = map #name $ getCandidateSources theAssociation
        candidateTargets = map #name $ getCandidateTargets theAssociation

        numberOfSources = length candidateSources
        numberOfTargets = length candidateTargets

        allPossibleLinks =  [ Link associationName i j | i <- candidateSources , j <- candidateTargets]

        multSourceMax = maybe numberOfTargets (min numberOfTargets) multTargetMaxMaybe
        multTargetMax = maybe numberOfSources (min numberOfSources) multSourceMaxMaybe

        in do
            let occurrencesAsSourceEmpty = fromList $ map (( , 0) . #name) theClasses :: Map Name Int
            let occurrencesAsTargetEmpty = fromList $ map (( , 0) . #name) theClasses :: Map Name Int
            shuffled <- shuffle allPossibleLinks
            let (result, _ , _ ) = foldl (\(soFar, occurrencesAsSource, occurrencesAsTarget) link@Link{source = theSource, target = theTarget} ->
                        if or
                            [ link `elem` soFar
                            , inverted link `elem` soFar
                            , occurrencesAsSource ! theSource >= multSourceMax
                            , occurrencesAsTarget ! theTarget >= multTargetMax
                            ]
                            then (soFar, occurrencesAsSource, occurrencesAsTarget)
                            else (link : soFar, adjust (+1) theSource occurrencesAsSource, adjust (+1) theTarget occurrencesAsTarget)
                    ) ([], occurrencesAsSourceEmpty, occurrencesAsTargetEmpty) shuffled
            return result
        -- constructLinks :: [(Name, Name)] -> [Link]
        -- constructLinks = map (uncurry (Link associationName))

        -- candidateTargets = map #name $ getCandidateTargets theAssociation :: [Name]
        -- candidateSources = map #name $ getCandidateSources theAssociation :: [Name]

        -- numberOfSources = length candidateSources :: Int
        -- numberOfTargets = length candidateTargets :: Int

        -- occurrencesSource = maybe numberOfTargets (min numberOfTargets) multTargetMaxMaybe
        -- occurrencesTarget = maybe numberOfSources (min numberOfSources) multSourceMaxMaybe

        -- representedSources = concatMap (replicate occurrencesSource) candidateSources
        -- representedTargets = concatMap (replicate occurrencesTarget) candidateTargets

        -- mustSwap = length representedSources > length representedTargets :: Bool

        -- (short, long) = (if mustSwap then swap else id) (representedSources, representedTargets) :: ([Name], [Name])

        -- -- achievableNumberOfLinks = length short :: Int

        -- bundleSize = if mustSwap then occurrencesTarget else occurrencesSource  :: Int

        -- in do
        --     ( _ , _ , ready) <-
        --         foldM (\(remaining, current0, result) s -> let
        --                 current = if length current0 == bundleSize
        --                     then []
        --                     else current0
        --                 canChooseFrom = filter (\x -> (s, x) `notElem` current && (x, s) `notElem` current) remaining
        --                 in if null canChooseFrom
        --                     then return (remaining, current, result)
        --                     else do
        --                         new <- elements canChooseFrom
        --                         return (delete new remaining, (s, new) : current, (s, new) : result)
        --         ) (long, [], []) short

        --     return $ nubOrd $ constructLinks $ (if mustSwap then map swap else id) ready

    in do
        allPossibleLinks <- concat <$> forM theAssociations addLinksForOneAssociation :: Gen [Link]
        let numberOfLinksToKeep = round $ portionOfPossibleLinksToKeep * fromIntegral (length allPossibleLinks) :: Int
        return $ take numberOfLinksToKeep allPossibleLinks

----------------------------------------------------------

generateMLM :: Config -> Gen MLM
generateMLM Config{ projectNameString, maxClassLevel, numberOfClasses, numberOfAssociations, tendencyToConcretize, tendencyToInherit, multiplicitySpecAssociations, chanceVisibleAssociation, tendencyAbstractClass, portionOfPossibleLinksToKeep, averageNumberOfAttributesPerClass, tendencyToDistanceAttributeFromItsInstantiation } = let

    projectName = Name projectNameString :: Name

    maxClassLevelSafe = max 0 maxClassLevel :: Level
    numberOfClassesSafe = max 1 numberOfClasses :: Int
    numberOfAssociationsSafe = max 0 numberOfAssociations :: Int

    endlessEmptyClasses = map (emptyClass <<<) classNameSpace :: [Class]
    endlessEmptyAssociations = map (emptyAssociation <<<) associationNameSpace :: [Association]

    emptyClasses = take numberOfClassesSafe endlessEmptyClasses :: [Class]
    emptyAssociations = take numberOfAssociationsSafe endlessEmptyAssociations :: [Association]

    in do
        withConcretizations <- addConcretizations maxClassLevelSafe tendencyToConcretize emptyClasses :: Gen [Class]
        withAbstractions <- addAbstractions tendencyAbstractClass withConcretizations :: Gen [Class]
        withInheritances <- addInheritances tendencyToInherit withAbstractions :: Gen [Class]
        withAttributes <- addAttributes averageNumberOfAttributesPerClass tendencyToDistanceAttributeFromItsInstantiation withInheritances :: Gen [Class]
        readyClasses <- addSlotValues withAttributes :: Gen [Class]

        readyAssociations <- addAssociations multiplicitySpecAssociations chanceVisibleAssociation withAttributes emptyAssociations :: Gen [Association]

        readyLinks <- addLinks portionOfPossibleLinksToKeep readyClasses readyAssociations

        return $ MLM projectName readyClasses readyAssociations readyLinks
