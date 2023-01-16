{-# LANGUAGE FunctionalDependencies #-}

module Modelling.MLM.Validate (
  valid,
) where

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
  Level,
  Leniencies,
  LeniencyConsideringConcretization(..),
  LeniencyConsideringSlotFilling(..),
  LeniencyConsideringLowerMultiplicities(..),
  (\?/),
  generateAboveFinder,
  generateClassDict,
  generateClassFinder,
  generateInstantiatableOperationsFinder,
  generateOccurrencesCounter,
  equalType,
  inRangeOfMult
  )

import Data.List.UniqueStrict (allUnique)
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (isNothing, isJust)
import qualified Data.Map.Strict as M (filter, null, map)
import Data.Map.Strict (Map, member, elems)

noCycles :: (Eq a, Ord a) => Map a [a] -> Bool
noCycles x = M.null x || peeled /= x && noCycles peeled
    where peeled = M.filter (not . null) $ M.map (filter (`member` x)) x

class Validatable context a | a -> context where
  valid :: context -> a -> Bool

instance Validatable () Name where
  valid () (Name name) = let
    validChar1 = flip elem (['a'..'z'] ++ ['A'..'Z'])
    validCharN char = validChar1 char || isDigit char || char == '_'
    in
      all ($ name)
        [not . null, validChar1 . head, all validCharN . tail]

instance Validatable Leniencies MLM where
  valid (requireInstantiations, requireAllSlots, requireMinimumLinks)
    MLM{name = projectName, classes, associations, links}
    = let

    -- navigation
    findClass :: Name -> Maybe Class
    findClass = generateClassFinder classes

    findAssociationOfLink :: Link -> Maybe Association
    findAssociationOfLink Link{association} = find ((association ==) . #name) associations

    dict :: Map Name [Name]
    dict = generateClassDict classes

    classesNames = map #name classes :: [Name]

    (\/) :: Name -> Name -> Bool
    (\/) = (\?/) (Right (classesNames, dict))

    above :: Name -> [Class]
    above = generateAboveFinder classes (Just (\/))

    -- whether an association multiplicity is violated:
    occurrencesAsSource :: Class -> Association -> Int
    occurrencesAsSource = generateOccurrencesCounter True links

    occurrencesAsTarget :: Class -> Association -> Int
    occurrencesAsTarget = generateOccurrencesCounter False links

    participationDoesNotViolateMultiplicity :: Association -> Class -> Bool
    participationDoesNotViolateMultiplicity association'@Association{multSource, multTarget, source, target, levelSource, levelTarget} class'@Class{name, level} =
        (not (name \/ source && level == levelSource)
         ||
         (class' `occurrencesAsSource` association') `inRangeOfMult`
          case (requireMinimumLinks, multTarget) of
            (BeStrictAboutLowerMultiplicities, _) -> multTarget
            (_, Multiplicity (_, upper)) -> Multiplicity (0, upper)
        )
        &&
        (not (name \/ target && level == levelTarget)
         ||
         (class' `occurrencesAsTarget` association') `inRangeOfMult`
          case (requireMinimumLinks, multSource) of
            (BeStrictAboutLowerMultiplicities, _) -> multSource
            (_, Multiplicity (_, upper)) -> Multiplicity (0, upper)
        )

    associationMultiplicityNotViolated :: Association -> Bool
    associationMultiplicityNotViolated association' =
      all (participationDoesNotViolateMultiplicity association') classes

    isLinked :: Name -> Bool
    isLinked className = any (\Link{source, target} -> source == className || target == className) links

    instantiatableOperations :: Class -> [Operation]
    instantiatableOperations = generateInstantiatableOperationsFinder above

    instantiatesSomethingOrIsMetaClass :: Class -> Bool
    instantiatesSomethingOrIsMetaClass c@Class{classifier, name, slots} =
      isNothing classifier ||
      isLinked name ||
      not (null slots) ||
      not (null (instantiatableOperations c))

    validLink :: Link -> Bool
    validLink link@Link{source = linkSource, target = linkTarget} =
      maybe False (\Association{source = associationSource, target = associationTarget, levelSource, levelTarget} ->
          and [
              linkSource \/ associationSource,
              linkTarget \/ associationTarget,
              maybe False ((== levelSource) . #level) (findClass linkSource),
              maybe False ((== levelTarget) . #level) (findClass linkTarget)
            ]
        ) (findAssociationOfLink link)

    in and [
      not (null classes),
      all (`elem` classesNames) (concat (elems dict)),
      valid () projectName,
      allUnique classesNames,
      allUnique (map #name associations),
      noCycles dict,
      requireInstantiations == BeLenientAboutConcretization
        || all instantiatesSomethingOrIsMetaClass classes,
      allUnique links,
      all (\x -> valid (requireAllSlots, above (#name x)) x) classes,
      all (\x -> valid (findClass (#source x), findClass (#target x)) x) associations,
      all associationMultiplicityNotViolated associations,
      all validLink links
    ]

instance Validatable (LeniencyConsideringSlotFilling, [Class]) Class where
  valid (requireAllSlots, aboveThis)
    Class{ isAbstract = thisAbstract
         , name = thisName
         , level = thisLevel
         , classifier = thisClassifier
         , attributes = thisAttributes
         , slots = thisSlots
         , operations = thisOperations
         , parents = thisParents}
    = let

    findClass :: Name -> Maybe Class
    findClass = generateClassFinder aboveThis

    allAttributesAbove = concatMap #attributes aboveThis :: [Attribute]

    instantiatableAttributesHere = filter ((== thisLevel) . #level) allAttributesAbove :: [Attribute]

    thisAttributesNames = map #name thisAttributes :: [Name]
    thisSlotsNames = map #attribute thisSlots :: [Name]
    thisOperationsNames = map #name thisOperations :: [Name]

    getAttributeOfSlot :: Slot -> Maybe Attribute
    getAttributeOfSlot Slot{attribute} = find ((attribute ==) . #name) allAttributesAbove

    in
    and [
        valid () thisName,
        valid () thisLevel,
        all (maybe False (\Class{level, classifier} -> level == thisLevel && classifier == thisClassifier) . findClass) thisParents,
        maybe True (maybe False (\Class{level, isAbstract} -> level == thisLevel + 1 && not isAbstract) . findClass) thisClassifier,
        allUnique thisParents,
        allUnique thisAttributesNames,
        allUnique thisSlotsNames,
        allUnique thisOperationsNames,
        allUnique (map (\Attribute{name, level} -> (name, level)) $ thisAttributes ++ allAttributesAbove),
        -- pretending that all attributes multiplicities are (1, Just 1)
        requireAllSlots == BeLenientAboutSlotFilling
          || all ((`elem` thisSlotsNames) . #name) instantiatableAttributesHere,
        thisLevel > 0 || (null thisAttributes && null thisOperations && null thisParents && not thisAbstract && isJust thisClassifier),
        all (valid thisLevel) thisAttributes,
        all (valid thisLevel) thisOperations,
        all (\x -> valid (getAttributeOfSlot x, thisLevel) x) thisSlots
        ]

instance Validatable Level Attribute where
  valid classLevel Attribute{multiplicity, level = attributeLevel, name} = and [
      valid () attributeLevel,
      valid () name,
      -- valid () multiplicity,
      -- enforcing the following specific value of multiplicities instead, because it is still not clear how to instantiate an attribute 0 times or >1 times
      multiplicity == Multiplicity (1, Just 1),
      classLevel > attributeLevel
    ]

instance Validatable (Maybe Attribute, Level) Slot where
  valid (slotAttribute, slotClassLevel) Slot{value} =
      maybe False (\x ->
          slotClassLevel == #level x && equalType (#dataType x) value
        ) slotAttribute

instance Validatable Level Operation where
  valid operationClassLevel Operation{level, name} =
    valid () name &&
    valid () level &&
    operationClassLevel > level

instance Validatable (Maybe Class, Maybe Class) Association where
  valid (sourceClass, targetClass) Association{levelSource, levelTarget, multSource, multTarget, name} =
    and [
      valid () name,
      maybe False ((> levelSource) . #level) sourceClass,
      maybe False ((> levelTarget) . #level) targetClass,
      valid () multSource,
      valid () multTarget
      ]

instance Validatable () Multiplicity where
  valid () (Multiplicity (lower, Nothing)) = lower >= 0
  valid () (Multiplicity (lower, Just upper)) = lower >= 0 && lower <= upper

instance Validatable () Level where
  valid () level = level >= 0
