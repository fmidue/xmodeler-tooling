{-# OPTIONS_GHC -Wno-orphans #-}

module Modelling.MLM.Types(
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
  Validatable,
  Type (..),
  Value (..),
  XModelerCurrency (..),
  valid,
  relativeToEur,
  currencySymbol,
  emptyName,
  emptyMLM,
  emptyClass,
  emptyAssociation,
  emptyLink,
  emptyAttribute,
  emptySlot,
  emptyOperation,
  attributeTypeSpace,
  generateClassDict,
  generateClassifierDict,
  generateParentDict,
  (\?/),
  generateScopeFinder,
  generateClassFinder,
  generateInstantiatableAttributesFinder
) where

import Data.List.UniqueStrict (allUnique)
import Data.Char (isDigit)
import Data.List (find, sort, sortOn)
import Data.Ix (inRange)
import Data.Maybe (isNothing, maybeToList, mapMaybe)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, member, (!), fromList)
import Data.List.Extra (replace, nubOrd)

noCycles :: (Eq a, Ord a) => Map a [a] -> Bool
noCycles x = M.null x || peeled /= x && noCycles peeled
    where peeled = M.filter (not . null) $ M.map (filter (`member` x)) x


eqBy :: Eq b => a -> a -> (a -> b) -> Bool
eqBy x y f = f x == f y

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

class Validatable context a where
  valid :: context -> a -> Bool

newtype Name = Name String deriving (Eq, Ord, Show, Read)

emptyName :: Name
emptyName = Name ""

instance Validatable () Name where
  valid () (Name name) = let
    validChar1 = flip elem (['a'..'z'] ++ ['A'..'Z'])
    validCharN char = validChar1 char || isDigit char || char == '_'
    in
      all ($ name)
        [not . null, validChar1 . head, all validCharN . tail]

data MLM = MLM {
  name :: Name,
  classes :: [Class],
  associations :: [Association],
  links :: [Link]
} deriving (Show, Read)

instance Eq MLM where
  (==) MLM{classes = c1, associations = a1, links = l1} MLM{classes = c2, associations = a2, links = l2} =
    sortOn #name c1 == sortOn #name c2 &&
    sortOn #name a1 == sortOn #name a2 &&
    sort l1 == sort l2

emptyMLM :: MLM
emptyMLM = MLM emptyName [] [] []

generateParentDict :: [Class] -> Map Name [Name]
generateParentDict = fromList . map (\Class{name, parents} -> (name, parents))

generateClassifierDict :: [Class] -> Map Name (Maybe Name)
generateClassifierDict = fromList . map (\Class{name, classifier} -> (name, classifier))

generateClassDict :: [Class] -> Map Name [Name]
generateClassDict = fromList . map (\Class{name, classifier, parents} -> (name, maybeToList classifier ++ parents))

(\?/) :: [Class] -> (Name -> Name -> Bool)
(\?/) theClasses a b = let
  classDict = generateClassDict theClasses :: Map Name [Name]
  f :: Name -> Name -> Bool
  f x y = let list = classDict ! x
    in y `elem` list || any (`f` y) list
  in f a b

generateInScopeFinder :: [Class] -> (Class -> [Class])
generateInScopeFinder theClasses x = let
  (\/) :: Name -> Name -> Bool
  (\/) = (\?/) theClasses
  in filter ((#name x \/) . #name) theClasses

generateScopeFinder :: [Class] -> (Class -> [Class])
generateScopeFinder theClasses x = let
  (\/) :: Name -> Name -> Bool
  (\/) = (\?/) theClasses
  in filter ((\/ #name x) . #name) theClasses

generateClassFinder :: [Class] -> (Name -> Maybe Class)
generateClassFinder theClasses x = find ((== x) . #name) theClasses

generateInstantiatableAttributesFinder :: [Class] -> (Class -> [Attribute])
generateInstantiatableAttributesFinder theClasses c@Class{level = classLevel} = let
  inScope :: Class -> [Class]
  inScope = generateInScopeFinder theClasses
  in filter ((== classLevel) . #level) $ concatMap #attributes $ inScope c

generateInstantiatableOperationsFinder :: [Class] -> (Class -> [Operation])
generateInstantiatableOperationsFinder theClasses c@Class{level = classLevel} = let
  inScope :: Class -> [Class]
  inScope = generateInScopeFinder theClasses
  in filter ((== classLevel) . #level) $ concatMap #operations $ inScope c

instance Validatable () MLM where
  valid () MLM{name = projectName, classes, associations, links} = let

    -- navigation
    getClass :: Name -> Maybe Class
    getClass = generateClassFinder classes

    getAssociation :: Link -> Maybe Association
    getAssociation Link{name = linkName} = find ((== linkName) . #name) associations

    parentDict :: Map Name [Name]
    parentDict = generateParentDict classes

    classifierDict :: Map Name (Maybe Name)
    classifierDict = generateClassifierDict classes

    dict :: Map Name [Name]
    dict = generateClassDict classes

    getClassifier :: Name -> Maybe Name
    getClassifier = (classifierDict !)

    (\/) :: Name -> Name -> Bool
    (\/) = (\?/) classes

    inScope :: Class -> [Class]
    inScope = generateInScopeFinder classes

    -- whether an association multiplicity is violated:
    candidates :: Name -> Level -> [Class]
    candidates className lvl =
      filter (\Class{level, name} -> level == lvl && name \/ className) classes

    numOfTimesAsSource :: Name -> Class -> Int
    numOfTimesAsSource associationName Class{name = className} =
      length $ filter (\Link{name, source} ->
        name == associationName &&
        source == className
      ) links

    numOfTimesAsTarget :: Name -> Class -> Int
    numOfTimesAsTarget associationName Class{name = className} =
      length $ filter (\Link{name, target} ->
        name == associationName &&
        target == className
      ) links

    inRangeOfMult :: Multiplicity -> Int -> Bool
    inRangeOfMult (Multiplicity (a, Nothing)) x = x >= a
    inRangeOfMult (Multiplicity (a, Just b)) x = inRange (a, b) x

    allSourcesAreLinkedAccordingToMultiplicity :: Association -> Bool
    allSourcesAreLinkedAccordingToMultiplicity Association{name = associationName, source, multSourceToTarget, lvlSource} = let
      candidatesHere :: [Class]
      candidatesHere = candidates source lvlSource
      numOfTimes :: Class -> Int
      numOfTimes = numOfTimesAsSource associationName
      in all (inRangeOfMult multSourceToTarget . numOfTimes) candidatesHere

    allTargetsAreLinkedAccordingToMultiplicity :: Association -> Bool
    allTargetsAreLinkedAccordingToMultiplicity Association{name = associationName, target, multTargetToSource, lvlTarget} = let
      candidatesHere :: [Class]
      candidatesHere = candidates target lvlTarget
      numOfTimes :: Class -> Int
      numOfTimes = numOfTimesAsTarget associationName
      in all (inRangeOfMult multTargetToSource . numOfTimes) candidatesHere

    multNotViolated :: Association -> Bool
    multNotViolated a = allTargetsAreLinkedAccordingToMultiplicity a && allSourcesAreLinkedAccordingToMultiplicity a

    -- whether a class does not concretize a class whose level is not higher by 1
    lvlIsClassifierLvlMinusOne class' =
      maybe True
        (maybe False
          (\classifierExisting -> #level classifierExisting == #level class' + 1)
          . getClass
      ) (#classifier class')

    isLinked :: Name -> Bool
    isLinked className = any (\Link{source, target} -> source == className || target == className) links

    instantiatableOperations :: Class -> [Operation]
    instantiatableOperations = generateInstantiatableOperationsFinder classes

    instantiatesSomethingOrIsMetaClass :: Class -> Bool
    instantiatesSomethingOrIsMetaClass c@Class{slots, classifier, name} =
      isNothing classifier ||
      isLinked name ||
      not (null slots) ||
      not (null (instantiatableOperations c))


    -- whether source of link concretizes or inherits from source of association of that link and
    -- whether target of link concretizes or inherits from target of association of that link
    checkSourceAndTarget :: Link -> Bool
    checkSourceAndTarget x = maybe False (\a ->
        #source x \/ #source a && #target x \/ #target a
      ) (getAssociation x)

    instantiatableAttributes :: Class -> [Attribute]
    instantiatableAttributes = generateInstantiatableAttributesFinder classes

    instantiatableAttributesAreInstantiated :: Class -> Bool
    instantiatableAttributesAreInstantiated c@Class{slots} = all ((`elem` map #name slots) . #name) (instantiatableAttributes c)

    allAttributesInScope :: Class -> [Attribute]
    allAttributesInScope x = #attributes x ++ concatMap #attributes (inScope x)

    -- allOperationsInScope :: Class -> [Operation]
    -- allOperationsInScope x = #operations x ++ concatMap #operations (inScope x)

    allClassifiers :: [Name]
    allClassifiers = nubOrd $ mapMaybe #classifier classes

    in and [
      not (null classes),
      valid () projectName,
      allUnique (map #name classes),
      allUnique (map #name associations),
      noCycles dict,
      all lvlIsClassifierLvlMinusOne classes,
      all instantiatesSomethingOrIsMetaClass classes,
      allUnique links,
      all (allUnique . map (\Attribute{name, level} -> (name, level)) . allAttributesInScope) classes,
      all (allUnique . map #name . #attributes) classes,
      -- all (allUnique . map (\Operation{name, level} -> (name, level)) . allOperationsInScope) classes,
      all (allUnique . map #name . #operations) classes,
      all instantiatableAttributesAreInstantiated classes,
      all (allUnique . map #name . #slots) classes,
      all (\Class{classifier = c, name} -> all ((== c) . getClassifier) (parentDict ! name) ) classes,
      all (\x -> valid (inScope x, map getClass (#parents x)) x) classes,
      all (\x -> valid (getClass (#source x), getClass (#target x)) x) associations,
      all multNotViolated associations,
      all checkSourceAndTarget links,
      all (\x -> valid
          (getClass (#source x),getClass (#target x))
          (getAssociation x)
        ) links,
      all (\Class{name, isAbstract} -> not isAbstract || name `notElem` allClassifiers) classes
    ]

data Class = Class {
  isAbstract :: Bool,
  level :: Level,
  name :: Name,
  parents :: [Name],
  classifier :: Maybe Name,
  attributes :: [Attribute],
  operations :: [Operation],
  slots :: [Slot]
} deriving (Show, Read)

instance Eq Class where
  (==) x y = and [
      eqBy x y #isAbstract,
      eqBy x y #level,
      eqBy x y #name,
      eqBy (#parents x) (#parents y) sort,
      eqBy x y #classifier,
      sortOn #name (#attributes x) == sortOn #name (#attributes y),
      sortOn #name (#operations x) == sortOn #name (#operations y),
      sortOn #name (#slots x) == sortOn #name (#slots y)
    ]

emptyClass :: Class
emptyClass = Class False 0 emptyName [] Nothing [] [] []

instance Validatable ([Class], [Maybe Class]) Class where
  valid (classInScope, parentsClasses) Class{name = className, level = level', attributes = attributes', slots, operations, parents} = let

    getAttributeClass :: Name -> Maybe Class
    getAttributeClass x = find (elem x . map #name . #attributes) classInScope

    getAttribute :: Name -> Maybe Attribute
    getAttribute x = ((find ((== x) . #name) . #attributes) =<< getAttributeClass x)

    in
    and [
        valid () className,
        valid () level',
        all (maybe False ((== level') . #level)) parentsClasses,
        allUnique parents,
        level' > 0 || (null attributes' && null operations && null parents),
        all (valid level') attributes',
        all (valid level') operations,
        all (\x -> valid (getAttribute (#name x) , level') x) slots
        ]

data Attribute = Attribute {
  level :: Level,
  name :: Name,
  dataType :: Type,
  multiplicity :: Multiplicity
} deriving (Show, Read, Eq)

emptyAttribute :: Attribute
emptyAttribute = Attribute 0 emptyName Boolean emptyMultiplicity

instance Validatable Level Attribute where
  valid classLevel Attribute{multiplicity, level = attributeLevel, name} = and [
      valid () attributeLevel,
      valid () name,
      valid () multiplicity,
      classLevel > attributeLevel
    ]

data Slot = Slot {
  name :: Name,
  value :: Value
} deriving (Show, Read, Eq)

emptySlot :: Slot
emptySlot = Slot emptyName emptyValue

instance Validatable (Maybe Attribute, Level) Slot where
  valid (slotAttribute, slotClassLvl) Slot{value} =
      maybe False (\x -> slotClassLvl == #level x && equalType (#dataType x) value) slotAttribute

data Operation = Operation {
  level :: Int,
  name :: Name,
  dataType :: Type,
  isMonitored :: Bool,
  body :: String
} deriving (Show, Read)

instance Eq Operation where
  (==) x y = and [
      eqBy x y #level,
      eqBy x y #name,
      eqBy x y #dataType,
      eqBy x y #isMonitored,
      eqBy x y (replace "&#10;" "\n" . #body)
    ]

emptyOperation :: Operation
emptyOperation = Operation 0 emptyName Boolean False ""

instance Validatable Level Operation where
  valid operationClassLvl Operation{level, name} =
    valid () name &&
    valid () level &&
    operationClassLvl > level

data Association = Association {
  name :: Name,
  source :: Name,
  target :: Name,
  lvlSource :: Level,
  lvlTarget :: Level,
  multTargetToSource :: Multiplicity,
  multSourceToTarget :: Multiplicity,
  sourceVisibleFromTarget :: Bool,
  targetVisibleFromSource :: Bool
} deriving (Show, Read, Eq)

emptyAssociation :: Association
emptyAssociation = Association emptyName emptyName emptyName 0 0 emptyMultiplicity emptyMultiplicity False False

instance Validatable (Maybe Class, Maybe Class) Association where
  valid (sourceClass, targetClass) Association{lvlSource, lvlTarget, multTargetToSource, multSourceToTarget, name} =
    and [
      valid () name,
      maybe False ((> lvlSource) . #level) sourceClass,
      maybe False ((> lvlTarget) . #level) targetClass,
      valid () multTargetToSource,
      valid () multSourceToTarget
      ]

data Link = Link {
  name :: Name,
  source :: Name,
  target :: Name
} deriving (Show, Read, Ord, Eq)

emptyLink :: Link
emptyLink = Link emptyName emptyName emptyName

instance Validatable (Maybe Class, Maybe Class) (Maybe Association) where
  valid (linkSource0, linkTarget0) =
    maybe False (\linkAssociation ->
      maybe False (\linkSource ->
        maybe False (\linkTarget ->
          #lvlSource linkAssociation == #level linkSource &&
          #lvlTarget linkAssociation == #level linkTarget
        ) linkTarget0
      ) linkSource0
    )

newtype Multiplicity = Multiplicity (Int, Maybe Int) deriving (Eq, Show, Read)

emptyMultiplicity :: Multiplicity
emptyMultiplicity = Multiplicity (0, Nothing)

instance Validatable () Multiplicity where
  valid () (Multiplicity (lower, Nothing)) = lower >= 0
  valid () (Multiplicity (lower, Just upper)) = lower >= 0 && lower <= upper

type Level = Int

instance Validatable () Level where
  valid () level = level >= 0

data Type = Boolean | Integer | Float | String | Element | MonetaryValue | Date | Currency | Complex | AuxiliaryClass deriving (Eq, Show, Read, Enum, Bounded)

data Value =
  VBoolean Bool |
  VInteger Int |
  VFloat Float |
  VString String |
  VElement String |
  VMonetaryValue (String, String) |
  VDate (Int, Int, Int) |
  VCurrency XModelerCurrency |
  VComplex String |
  VAuxiliaryClass String
  deriving (Eq, Show, Read)

emptyValue :: Value
emptyValue = VBoolean False

attributeTypeSpace :: [Type]
attributeTypeSpace = [minBound .. maxBound]

equalType :: Type -> Value -> Bool
equalType Boolean (VBoolean _) = True
equalType Integer (VInteger _) = True
equalType Float (VFloat _) = True
equalType String (VString _) = True
equalType Element (VElement _) = True
equalType MonetaryValue (VMonetaryValue _) = True
equalType Date (VDate _) = True
equalType Currency (VCurrency _) = True
equalType Complex (VComplex _) = True
equalType AuxiliaryClass (VAuxiliaryClass _) = True
equalType _ _ = False

data XModelerCurrency = USD | EUR | GBP | AUD | NZD deriving (Eq, Show, Read)

relativeToEur :: XModelerCurrency -> Float
relativeToEur USD = 0.9502091
relativeToEur EUR = 1.0
relativeToEur GBP = 1.165868
relativeToEur AUD = 0.65582377
relativeToEur NZD = 0.5976929

currencySymbol :: XModelerCurrency -> String
currencySymbol EUR = "EUR"
currencySymbol GBP = "£"
currencySymbol _ = "$"
