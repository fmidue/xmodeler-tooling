module Modelling.MLM.Edit (
    Edit,
    editRandomlyValidly,
    editRandomlyValidlyN,
    editRandomlyBlindly,
    editRandomlyBlindlyNKeepLastValidOne,
    editRandomlyBlindlyNStayValid
    ) where

import Modelling.MLM.Types (
  MLM(..),
  Link(..),
  Association(..),
  Class(..),
  Operation(..),
  Attribute(..),
  Slot(..),
  Multiplicity(..),
  Name(..),
  Type(..),
  typeSpace,
  generateBelowFinder,
  emptyOperationBody,
  valid,
  generateInstantiatableAttributesFinder,
  generateOccurrencesCounter,
  inRangeOfMult
  )
import Modelling.MLM.GenerateMLM (
    classNameSpace,
    associationNameSpace,
    attributeNameSpace,
    randomSlot,
    operationNameSpace,
    randomWeightedXOr,
    randomMult
    )
import Modelling.MLM.Config (Config(..))
import Modelling.MLM.Modify ((<<<), insert)

import Test.QuickCheck (Gen, elements, chooseAny, chooseInt, sublistOf)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)

data Edit = AddClass | AddAssociation | AddLink | AddAttribute | AddOperation | DeleteClass | DeleteAssociation | DeleteLink | DeleteAttribute | DeleteOperation deriving (Enum, Bounded)

generateNextAvailableClassNameFinder :: MLM -> Name
generateNextAvailableClassNameFinder MLM{classes} = fromMaybe (error "Impossible to not find an available class name in the infinite list of names!!!") $
    find (`notElem` map #name classes) classNameSpace

generateNextAvailableAssociationNameFinder :: MLM -> Name
generateNextAvailableAssociationNameFinder MLM{associations} = fromMaybe (error "Impossible to not find an available association name in the infinite list of names!!!") $
    find (`notElem` map #name associations) associationNameSpace

generateNextAvailableAttributeNameFinder :: MLM -> Name
generateNextAvailableAttributeNameFinder MLM{classes} = fromMaybe (error "Impossible to not find an available attribute name in the infinite list of names!!!") $
    find (`notElem` map #name (concatMap #attributes classes)) attributeNameSpace

generateNextAvailableOperationNameFinder :: MLM -> Name
generateNextAvailableOperationNameFinder MLM{classes} = fromMaybe (error "Impossible to not find an available operation name in the infinite list of names!!!") $
    find (`notElem` map #name (concatMap #operations classes)) operationNameSpace

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

editRandomlyValidly :: Bool -> Config -> MLM -> Gen MLM
editRandomlyValidly shouldWeForbidDeleteComponents config mlm = let
    editsToUse = if shouldWeForbidDeleteComponents
        then [AddClass, AddAssociation, AddLink, AddAttribute, AddOperation]
        else [minBound .. maxBound]
    in editValidly config mlm =<< elements editsToUse

editRandomlyValidlyN :: Bool -> Config -> MLM -> Int -> Gen MLM
editRandomlyValidlyN shouldWeForbidDeleteComponents config mlm n = let
    f = editRandomlyValidly shouldWeForbidDeleteComponents config
    in foldM (\soFar _ -> f soFar) mlm [1 .. n]

editValidly :: Config -> MLM -> Edit -> Gen MLM
editValidly Config{ maxClassLevel, tendencyToConcretize, tendencyToInherit, multiplicitySpecAssociations, chanceVisibleAssociation, tendencyAbstractClass, allowMultipleInheritance } mlm@MLM{classes, associations, links} e = let

    nextAvailableClassName :: Name
    nextAvailableClassName = generateNextAvailableClassNameFinder mlm

    nextAvailableAssociationName :: Name
    nextAvailableAssociationName = generateNextAvailableAssociationNameFinder mlm

    nextAvailableAttributeName :: Name
    nextAvailableAttributeName = generateNextAvailableAttributeNameFinder mlm

    nextAvailableOperationName :: Name
    nextAvailableOperationName = generateNextAvailableOperationNameFinder mlm

    randomType :: Gen Type
    randomType = elements typeSpace

    nonLevelZeroClasses = filter ((> 0) . #level) classes :: [Class]
    nonAbstractNonLevelZeroClasses = filter (not . #isAbstract) nonLevelZeroClasses :: [Class]

    below :: Name -> [Class]
    below className = maybe [] (generateBelowFinder classes) $ find ((== className) . #name) classes

    instantiatableAttributes :: MLM -> (Class -> [Attribute])
    instantiatableAttributes x = generateInstantiatableAttributesFinder $ #classes x

    occurrencesAsSource :: Class -> Association -> Int
    occurrencesAsSource = generateOccurrencesCounter True links

    occurrencesAsTarget :: Class -> Association -> Int
    occurrencesAsTarget = generateOccurrencesCounter False links

    refreshInstantiationOneClass :: MLM -> Class -> Gen Class
    refreshInstantiationOneClass mlm' class' =
        foldM (\soFar attribute ->
                if #name attribute `elem` map #name (#slots soFar)
                    then return soFar
                    else (soFar <<<) <$> randomSlot attribute
            ) class' (instantiatableAttributes mlm' class')

    refreshInstantiationAllClasses :: MLM -> Gen MLM
    refreshInstantiationAllClasses mlm'@MLM{classes = classes'} = do
        classes'' <- mapM (refreshInstantiationOneClass mlm') classes'
        return $ mlm'{classes = classes''}

    candidateSources :: Association -> [Class]
    candidateSources a = filter (\class' ->
            #level class' == #lvlSource a && (class' `occurrencesAsSource` a + 1) `inRangeOfMult` #multTarget a
        ) $ below $ #source a

    candidateTargets :: Association -> [Class]
    candidateTargets a = filter (\class' ->
            #level class' == #lvlTarget a && (class' `occurrencesAsTarget` a + 1) `inRangeOfMult` #multSource a
        ) $ below $ #target a


    allPossibleLinks :: Association -> [Link]
    allPossibleLinks a = let
        candidateSourcesNames = map #name $ candidateSources a
        candidateTargetsNames = map #name $ candidateTargets a
        in filter (`notElem` #links mlm) [Link (#name a) i j | i <- candidateSourcesNames, j <- candidateTargetsNames]

    exceptOnesNamed list x = filter ((/= x) . #name) list

    in case e of
        AddClass -> do
            let name' = nextAvailableClassName
            classifierClass <- if null nonAbstractNonLevelZeroClasses
                then return Nothing
                else randomWeightedXOr tendencyToConcretize
                    (Just <$> elements nonAbstractNonLevelZeroClasses)
                    (return Nothing)
            let classifier' = #name <$> classifierClass
            level' <- maybe (chooseInt (1, maxClassLevel)) (return . subtract 1 . #level) classifierClass
            isAbstract' <- if level' < 1
                then return False
                else randomWeightedXOr tendencyAbstractClass (return True) (return False)
            let canBeParents = filter (\x -> #level x == level' && #classifier x == classifier') classes
            parents' <- if level' < 1 || null canBeParents
                then return []
                else if allowMultipleInheritance
                    then randomWeightedXOr tendencyToInherit
                        (map #name <$> sublistOf canBeParents)
                        (return [])
                    else (:[]) . #name <$> elements canBeParents
            let classToBeAdded = Class isAbstract' level' name' parents' classifier' [] [] []
            let mlmWithTheClassEvenIfNotReadyYet = mlm <<< classToBeAdded
            -- if it cannot instantiate any attributes and it is not a meta class, then let's add one for it in its classifier (if it exists) and let it instantiate it with the rest of instantiable attributes:
            mlmWithTheNewAttributeMaybe <- do
                dataType' <- randomType
                let newAttribute = Attribute level' nextAvailableAttributeName dataType' (Multiplicity (1, Just 1))
                return $ maybe mlmWithTheClassEvenIfNotReadyYet (\c -> insert mlmWithTheClassEvenIfNotReadyYet c newAttribute) classifier'
            refreshInstantiationAllClasses mlmWithTheNewAttributeMaybe
        AddAssociation -> if null nonLevelZeroClasses then return mlm else do
            let name' = nextAvailableAssociationName
            sourceClass <- elements nonLevelZeroClasses
            targetClass <- elements nonLevelZeroClasses
            let source' = #name sourceClass
            let target' = #name targetClass
            lvlSource' <- chooseInt (0, #level sourceClass - 1)
            lvlTarget' <- chooseInt (0, #level targetClass - 1)
            multSource' <- randomMult multiplicitySpecAssociations
            multTarget' <- randomMult multiplicitySpecAssociations
            visibleSource' <- randomWeightedXOr chanceVisibleAssociation (return True) (return False)
            visibleTarget' <- randomWeightedXOr chanceVisibleAssociation (return True) (return False)
            let mlmWithTheAssociation = mlm <<< Association name' source' target' lvlSource' lvlTarget' multSource' multTarget' visibleSource' visibleTarget'
            return mlmWithTheAssociation
        AddLink -> let
            allPossibleLinksAllAssociations = concatMap allPossibleLinks associations
            in if null allPossibleLinksAllAssociations
                then return mlm
                else do
                    (mlm <<<) <$> elements allPossibleLinksAllAssociations
        AddAttribute -> if null nonLevelZeroClasses then return mlm else do
            let name' = nextAvailableAttributeName
            dataType' <- randomType
            let multiplicity' = Multiplicity (1, Just 1)
            class' <- elements nonLevelZeroClasses
            level' <- chooseInt (0, #level class' - 1)
            let attribute = Attribute level' name' dataType' multiplicity'
            let mlmWithTheAttribute = insert mlm (#name class') attribute
            refreshInstantiationAllClasses mlmWithTheAttribute
        AddOperation -> if null nonLevelZeroClasses then return mlm else do
            let name' = nextAvailableOperationName
            dataType' <- randomType
            isMonitored' <- chooseAny :: Gen Bool
            let body' = emptyOperationBody name'
            class' <- elements nonLevelZeroClasses
            level' <- chooseInt (0, #level class' - 1)
            let operation = Operation level' name' dataType' isMonitored' body'
            return $ insert mlm (#name class') operation
        DeleteClass -> if null classes then return mlm else do
            randomClass <- elements classes
            let randomClassName = #name randomClass
            let mlmWithoutTheClass = mlm{classes = classes `exceptOnesNamed` randomClassName}
            return mlmWithoutTheClass{
                    classes = map (\c@Class{slots} -> c{slots = filter (`notElem` #slots randomClass) slots}) (filter (\Class{classifier} -> Just randomClassName /= classifier) classes),
                    associations = filter (\Association{source, target} -> source /= randomClassName && target /= randomClassName) associations,
                    links = filter (\Link{source, target} -> source /= randomClassName && target /= randomClassName) links
                }
        DeleteAssociation -> if null associations then return mlm else do
            randomAssociation <- elements $ map #name associations
            let mlmWithoutTheAssociation = mlm{associations = associations `exceptOnesNamed` randomAssociation}
            return mlmWithoutTheAssociation{
                links = filter ((/= randomAssociation) . #name) links
            }
        DeleteLink -> if null links then return mlm else do
            randomLink <- elements $ map #name links
            return $ mlm{links = links `exceptOnesNamed` randomLink}
        DeleteAttribute -> if null (concatMap #attributes classes) then return mlm else do
            randomAttribute <- elements $ map #name $ concatMap #attributes classes
            let mlmWithoutTheAttribute = mlm{classes = map (\x@Class{attributes} -> x{attributes = attributes `exceptOnesNamed` randomAttribute}) classes}
            return $ mlmWithoutTheAttribute{
                    classes = map (\class'@Class{attributes} ->
                            class'{attributes = filter ((/= randomAttribute) . #name) attributes}
                        ) classes
                }
        DeleteOperation -> if null (concatMap #operations classes) then return mlm else do
            randomOperation <- elements $ map #name $ concatMap operations classes
            return $ mlm{classes = map (\x@Class{operations} -> x{operations = operations `exceptOnesNamed` randomOperation}) classes}

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

editRandomlyBlindly :: Bool -> MLM -> Gen MLM
editRandomlyBlindly shouldWeForbidDeleteComponents mlm = let
    editsToUse = if shouldWeForbidDeleteComponents
        then [AddClass, AddAssociation, AddLink, AddAttribute, AddOperation]
        else [minBound .. maxBound]
    in editBlindly mlm =<< elements editsToUse

editRandomlyBlindlyNKeepLastValidOne :: Bool -> MLM -> Int -> Gen (Maybe MLM)
editRandomlyBlindlyNKeepLastValidOne shouldWeForbidDeleteComponents mlm n = let
    f = editRandomlyBlindly shouldWeForbidDeleteComponents
    in do
        result <- sequence $ scanl (\soFar _ -> do
                    soFar' <- soFar
                    f soFar'
                ) (return mlm) [1 .. n] :: Gen [MLM]
        let validOnes = filter (valid ()) result
        if null validOnes
            then return Nothing
            else return $ Just $ last validOnes

editRandomlyBlindlyNStayValid :: Bool -> MLM -> Int -> Gen (Maybe MLM)
editRandomlyBlindlyNStayValid shouldWeForbidDeleteComponents mlm n = let
    f = editRandomlyBlindly shouldWeForbidDeleteComponents
    in do
        result <- foldM (\x _ -> do
                y <- f x
                return $ if valid () y then y else x
            ) mlm [1 .. n]
        if valid () result
            then return $ Just result
            else return Nothing

editBlindly :: MLM -> Edit -> Gen MLM
editBlindly mlm@MLM{classes, associations, links} e = let

    nextAvailableClassName = generateNextAvailableClassNameFinder mlm
    nextAvailableAssociationName = generateNextAvailableAssociationNameFinder mlm
    nextAvailableAttributeName = generateNextAvailableAttributeNameFinder mlm
    nextAvailableOperationName = generateNextAvailableOperationNameFinder mlm
    randomType = elements typeSpace

    exceptOnesNamed list x = filter ((/= x) . #name) list

    in case e of
        AddClass -> do
            let name' = nextAvailableClassName
            isAbstract' <- chooseAny :: Gen Bool
            classifierClass <- if null classes
                then return Nothing
                else Just <$> elements classes
            let classifier' = #name <$> classifierClass
            level' <- chooseInt (0, 10)
            parents' <- map #name <$> sublistOf classes
            let attributes' = []
            let operations' = []
            let slots' = []
            return $ mlm <<< Class isAbstract' level' name' parents' classifier' attributes' operations' slots'
        AddAssociation -> if null classes then return mlm else do
            let name' = nextAvailableAssociationName
            sourceClass <- elements classes
            targetClass <- elements classes
            let source' = #name sourceClass
            let target' = #name targetClass
            lvlSource' <- chooseInt (0, 10)
            lvlTarget' <- chooseInt (0, 10)
            multSource' <- randomMult (0, 3)
            multTarget' <- randomMult (0, 3)
            visibleSource' <- chooseAny :: Gen Bool
            visibleTarget' <- chooseAny :: Gen Bool
            return $ mlm <<< Association name' source' target' lvlSource' lvlTarget' multSource' multTarget' visibleSource' visibleTarget'
        AddLink -> if null associations || null classes then return mlm else do
            association <- elements associations
            let name' = #name association
            source' <- #name <$> elements classes
            target' <- #name <$> elements classes
            return $ mlm <<< Link name' source' target'
        AddAttribute -> if null classes then return mlm else do
            let name' = nextAvailableAttributeName
            dataType' <- randomType
            let multiplicity' = Multiplicity (1, Just 1)
            class' <- elements classes
            level' <- chooseInt (0, 10)
            let attribute = Attribute level' name' dataType' multiplicity'
            return $ mlm{classes = map (\x ->
                    if #name x == #name class'
                        then x <<< attribute
                        else x
                ) classes}
        AddOperation -> if null classes then return mlm else do
            let name' = nextAvailableOperationName
            dataType' <- randomType
            isMonitored' <- chooseAny :: Gen Bool
            let body' = emptyOperationBody name'
            class' <- elements classes
            level' <- chooseInt (0, 10)
            let operation = Operation level' name' dataType' isMonitored' body'
            return $ mlm{classes = map (\x ->
                    if #name x == #name class'
                        then x <<< operation
                        else x
                ) classes}
        DeleteClass -> if null classes then return mlm else do
            randomClass <- elements $ map #name classes
            return $ mlm{classes = classes `exceptOnesNamed` randomClass}
        DeleteAssociation -> if null associations then return mlm else do
            randomAssociation <- elements $ map #name associations
            return $ mlm{associations = associations `exceptOnesNamed` randomAssociation}
        DeleteLink -> if null links then return mlm else do
            randomLink <- elements $ map #name links
            return $ mlm{links = links `exceptOnesNamed` randomLink}
        DeleteAttribute -> if null (concatMap #attributes classes) then return mlm else do
            randomAttribute <- elements $ map #name $ concatMap #attributes classes
            return $ mlm{classes = map (\x@Class{attributes} ->
                    x{attributes = attributes `exceptOnesNamed` randomAttribute})
                classes}
        DeleteOperation -> if null (concatMap #operations classes) then return mlm else do
            randomOperation <- elements $ map #name $ concatMap #operations classes
            return $ mlm{classes = map (\x@Class{operations} ->
                    x{operations = operations `exceptOnesNamed` randomOperation}
                ) classes}