module Modelling.MLM.GenerateMLM (generateMLM) where

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
  attributeTypeSpace,
  generateScopeFinder,
  generateClassFinder,
  generateInstantiatableAttributesFinder
  )
import Modelling.MLM.Modify ((<<<), SourceOrTarget(..))
import Test.QuickCheck (elements, choose, chooseAny, chooseInt, frequency, sublistOf, Gen)
import Control.Monad.Extra (concatMapM)
import Control.Monad (forM, foldM)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map, keys, (!), delete, fromList, adjust)
import qualified Data.Map.Strict as M

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

nonLevelZero :: [Class] -> [Class]
nonLevelZero = filter ((>0) . #level)

nonAbstract :: [Class] -> [Class]
nonAbstract = filter (not . #isAbstract)

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

-- there must be at least one class of level 0.
normalizeClassLevels :: [Class] -> [Class]
normalizeClassLevels classes = let lowest = minimum $ map #level classes in
    map (\x -> x <<< (#level x - lowest)) classes

randomMultGen :: (Float, Int) -> Gen Multiplicity
randomMultGen (chance, max') = do
    b <- randomWeightedXOr chance (Just <$> chooseInt (1, max')) (return Nothing)
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

addAbstractions :: Level -> Float -> [Class] -> Gen [Class]
addAbstractions maxLevel chanceAbstract theClasses = let
    -- spine is a chain of classes from level 0 to maxLevel that will be concretized later by other classes.
    -- This is why they must be concrete (not abstract):
    spine = map (<<< False) $ take (maxLevel + 1) theClasses :: [Class]
    meat = drop (maxLevel + 1) theClasses :: [Class]
    in (++) spine <$>
        mapM
            (\c -> do
                abstractOrNot <- randomWeightedXOr chanceAbstract (return True) (return False)
                return $ c <<< abstractOrNot
                )
            meat

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
        torso <- accumulate spine meat (\soFar x -> do
                let canConcretize = nonAbstract $ nonLevelZero soFar :: [Class]
                toConcretize <- randomWeightedXOr
                    chanceToConcretize
                    (if null canConcretize then return Nothing else  Just <$> elements canConcretize)
                    (return Nothing)
                x `concretizing` toConcretize
            ) :: Gen [Class]
        return $ normalizeClassLevels torso



addInheritances :: Float -> [Class] -> Gen [Class]
addInheritances chanceToInherit theClasses = let

    sameAs :: Class -> Class -> Bool
    sameAs Class{level = l1, classifier = c1}
         Class {level = l2, classifier = c2} = l1 == l2 && c1 == c2

    in accumulate [head theClasses] (tail theClasses) (\soFar x -> do
            toInheritFrom <- randomWeightedXOr
                chanceToInherit
                (if #level x > 0 then sublistOf (map #name (filter (sameAs x) soFar)) else return [])
                (return [])
            return $ x <<< (toInheritFrom :: [Name])
            )


addAttributes :: (Float, Int) -> [Class] -> Gen [Class]
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


addSlotValues :: [Class] -> Gen [Class]
addSlotValues theClasses = let

    instantiatable :: Class -> [Attribute]
    instantiatable = generateInstantiatableAttributesFinder theClasses

    addSlotValue :: Class -> Gen Class
    addSlotValue c =
        (c <<<) <$> mapM randomSlotValue (instantiatable c)

    in mapM addSlotValue theClasses


addAssociations :: (Float, Int) -> Float -> [Class] -> [Association] -> Gen [Association]
addAssociations multSpecs visibilityChance theClassesIncludingLevelZero emptyAssociations = let
    theClasses = nonLevelZero theClassesIncludingLevelZero :: [Class]
    randomClassGen = elements theClasses :: Gen Class
    randomLevelGen x = chooseInt (0, #level x - 1) :: Gen Level
    randomMultGen' = randomMultGen multSpecs
    randomVisibilityGen = randomWeightedXOr
        visibilityChance (return True) (return False) :: Gen Bool
    in forM emptyAssociations (\x -> do
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

addLinks :: Float -> [Class] -> [Association] -> Gen [Link]
addLinks portionOfPossibleLinksToKeep theClasses theAssociations = let

    scope :: Class -> [Class]
    scope = generateScopeFinder theClasses

    getClass :: Name -> Maybe Class
    getClass = generateClassFinder theClasses

    candidateSources :: Association -> [Class]
    candidateSources Association{source, lvlSource} =
        maybe [] (filter ((== lvlSource) . #level) . scope) (getClass source)

    candidateTargets :: Association -> [Class]
    candidateTargets Association{target, lvlTarget} =
        maybe [] (filter ((== lvlTarget) . #level) . scope) (getClass target)

    consume :: Name -> Map Name Int -> Map Name Int
    consume k map' = if map' ! k < 2
        then delete k map'
        else adjust (subtract 1) k map'

    marryCoupleNoDuplicate :: Name -> (Map Name Int, Map Name Int, Map Name [Name], [Link]) -> Gen [Link]
    marryCoupleNoDuplicate associationName (maleAvailability, femaleAvailability, marriageDict, linksSofar) =
        if M.null maleAvailability || M.null femaleAvailability then return linksSofar else do
            m <- elements $ keys maleAvailability :: Gen Name
            let fs = filter (`notElem` marriageDict ! m) $ keys femaleAvailability :: [Name]
            if null fs then return linksSofar else do
                f <- elements fs :: Gen Name
                marryCoupleNoDuplicate associationName (
                    consume m maleAvailability,
                    consume f femaleAvailability,
                    adjust (f :) m marriageDict,
                    Link associationName m f : linksSofar
                    )

    addLinksForOneAssociation :: Association -> Gen [Link]
    addLinksForOneAssociation a@Association{name = associationName, multSource = Multiplicity (_, multSourceMax0), multTarget = Multiplicity (_, multTargetMax0)} = let

        candidateTargetsHere = candidateTargets a :: [Class]
        candidateSourcesHere = candidateSources a :: [Class]

        multSourceMax = fromMaybe (length candidateSourcesHere) multSourceMax0 :: Int
        multTargetMax = fromMaybe (length candidateTargetsHere) multTargetMax0 :: Int

        sourcesNames = map #name candidateSourcesHere :: [Name]
        targetsNames = map #name candidateTargetsHere :: [Name]

        targetsAvailability = fromList $ map ( , multSourceMax) targetsNames :: Map Name Int
        sourcesAvailability = fromList $ map ( , multTargetMax) sourcesNames :: Map Name Int

        emptyDictForSources = fromList $ map ( , []) sourcesNames :: Map Name [Name]

        in marryCoupleNoDuplicate associationName (sourcesAvailability, targetsAvailability, emptyDictForSources, [])

    in do
        allPossibleLinks <- concatMapM addLinksForOneAssociation theAssociations
        let numberOfLinksToKeep = round $ portionOfPossibleLinksToKeep * fromIntegral (length allPossibleLinks) :: Int
        return $ take numberOfLinksToKeep allPossibleLinks

generateMLM :: Config -> Gen MLM
generateMLM Config{ projectNameString, maxClassLevel, numberOfClasses, numberOfAssociations, chanceToConcretize, chanceToInherit, multiplicitySpecAttributes, multiplicitySpecAssociations, chanceVisibleAssociation, chanceAbstractClass, portionOfPossibleLinksToKeep } = let

    projectName = Name projectNameString :: Name

    maxClassLevelSafe = max 0 maxClassLevel :: Level
    numberOfClassesSafe = max 1 numberOfClasses :: Int
    numberOfAssociationsSafe = max 0 numberOfAssociations :: Int

    endlessEmptyClasses = map (emptyClass <<<) classNameSpace :: [Class]
    endlessEmptyAssociations = map (emptyAssociation <<<) associationNameSpace :: [Association]

    emptyClasses = take numberOfClassesSafe endlessEmptyClasses :: [Class]
    emptyAssociations = take numberOfAssociationsSafe endlessEmptyAssociations :: [Association]

    in do
        withAbstractions <- addAbstractions maxClassLevelSafe chanceAbstractClass emptyClasses :: Gen [Class]
        withConcretizations <- addConcretizations maxClassLevelSafe chanceToConcretize withAbstractions :: Gen [Class]
        withInheritances <- addInheritances chanceToInherit withConcretizations :: Gen [Class]
        withAttributes <- addAttributes multiplicitySpecAttributes withInheritances :: Gen [Class]
        readyClasses <- addSlotValues withAttributes :: Gen [Class]

        readyAssociations <- addAssociations multiplicitySpecAssociations chanceVisibleAssociation withAttributes emptyAssociations :: Gen [Association]

        readyLinks <- addLinks portionOfPossibleLinksToKeep readyClasses readyAssociations

        return $ MLM projectName readyClasses readyAssociations readyLinks
