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
  emptyOperationBody,
  typeSpace,
  generateClassDict,
  generateClassifierDict,
  generateParentDict,
  (\?/),
  generateBelowFinder,
  generateClassFinder,
  generateInstantiatableAttributesFinder,
  generateAssociationFinder,
  generateOccurrencesCounter,
  inRangeOfMult
) where

import Data.List.UniqueStrict (allUnique)
import Data.Char (isDigit)
import Data.List (find, sort, sortOn)
import Data.Ix (inRange)
import Data.Maybe (isNothing, maybeToList, mapMaybe, fromMaybe)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, member, (!?), fromList)
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
    eqBy c1 c2 (sortOn #name) &&
    eqBy a1 a2 (sortOn #name) &&
    eqBy l1 l2 sort

emptyMLM :: MLM
emptyMLM = MLM emptyName [] [] []

generateParentDict :: [Class] -> Map Name [Name]
generateParentDict = fromList . map (\Class{name, parents} -> (name, parents))

generateClassifierDict :: [Class] -> Map Name Name
generateClassifierDict = fromList . concatMap (\Class{name, classifier} -> maybe [] ((:[]) . (name , )) classifier)

generateClassDict :: [Class] -> Map Name [Name]
generateClassDict theClasses = let x = fromList $ map (\Class{name, classifier, parents} -> (name, maybeToList classifier ++ parents)) theClasses
  in if M.size x /= length theClasses then error "Something is wrong with Map library" else x

(\?/) :: [Class] -> (Name -> Name -> Bool)
(\?/) theClasses a b = let
  classDict = generateClassDict theClasses :: Map Name [Name]
  f :: Name -> Name -> Bool
  f x y = let list = fromMaybe (error "There must be an entry in the dictionary, even if it is just an empty list. Maybe you are looking up a class that is not in the MLM!!!") (classDict !? x)
    in y `elem` list || any (`f` y) list
  in f a b

generateAboveFinder :: [Class] -> (Class -> [Class])
generateAboveFinder theClasses x = let
  (\/) :: Name -> Name -> Bool
  (\/) = (\?/) theClasses
  in filter ((#name x \/) . #name) theClasses

generateBelowFinder :: [Class] -> (Class -> [Class])
generateBelowFinder theClasses x = let
  (\/) :: Name -> Name -> Bool
  (\/) = (\?/) theClasses
  in filter ((\/ #name x) . #name) theClasses

generateClassFinder :: [Class] -> (Name -> Maybe Class)
generateClassFinder theClasses x = find ((== x) . #name) theClasses

generateAssociationFinder :: [Association] -> (Name -> Maybe Association)
generateAssociationFinder theAssociations x = find ((== x) . #name) theAssociations


generateInstantiatableAttributesFinder :: [Class] -> (Class -> [Attribute])
generateInstantiatableAttributesFinder theClasses c@Class{level = classLevel} = let
  above :: Class -> [Class]
  above = generateAboveFinder theClasses
  in filter ((== classLevel) . #level) $ concatMap #attributes $ above c

generateInstantiatableOperationsFinder :: [Class] -> (Class -> [Operation])
generateInstantiatableOperationsFinder theClasses c@Class{level = classLevel} = let
  above :: Class -> [Class]
  above = generateAboveFinder theClasses
  in filter ((== classLevel) . #level) $ concatMap #operations $ above c

generateOccurrencesCounter :: Bool -> [Link] -> (Class -> Association -> Int)
generateOccurrencesCounter asSourceRatherThanAsTarget theLinks Class{name = className} Association{name = associationName} = let
  toLookFor = if asSourceRatherThanAsTarget then #source else #target in
    length $ filter (\link ->
        #name link == associationName && toLookFor link == className
      ) theLinks

inRangeOfMult :: Int -> Multiplicity -> Bool
inRangeOfMult x (Multiplicity (a, Nothing)) = x >= a
inRangeOfMult x (Multiplicity (a, Just b)) = inRange (a, b) x

instance Validatable () MLM where
  valid () MLM{name = projectName, classes, associations, links} = let

    -- navigation
    findClass :: Name -> Maybe Class
    findClass = generateClassFinder classes

    findAssociationOfLink :: Link -> Maybe Association
    findAssociationOfLink Link{name = linkName} = find ((== linkName) . #name) associations

    parentDict :: Map Name [Name]
    parentDict = generateParentDict classes

    classifierDict :: Map Name Name
    classifierDict = generateClassifierDict classes

    dict :: Map Name [Name]
    dict = generateClassDict classes

    getClassifier :: Name -> Maybe Name
    getClassifier = (classifierDict !?)

    (\/) :: Name -> Name -> Bool
    (\/) = (\?/) classes

    above :: Class -> [Class]
    above = generateAboveFinder classes

    -- whether an association multiplicity is violated:
    occurrencesAsSource :: Class -> Association -> Int
    occurrencesAsSource = generateOccurrencesCounter True links

    occurrencesAsTarget :: Class -> Association -> Int
    occurrencesAsTarget = generateOccurrencesCounter False links

    participationDoesNotViolateMultiplicity :: Association -> Class -> Bool
    participationDoesNotViolateMultiplicity association'@Association{multSource, multTarget, source, target, lvlSource, lvlTarget} class'@Class{name, level} =
        (not (name \/ source && level == lvlSource) || (class' `occurrencesAsSource` association') `inRangeOfMult` multTarget)
        &&
        (not (name \/ target && level == lvlTarget) || (class' `occurrencesAsTarget` association') `inRangeOfMult` multSource)

    associationMultiplicityNotViolated :: Association -> Bool
    associationMultiplicityNotViolated association' =
      all (participationDoesNotViolateMultiplicity association') classes

    -- whether a class does not concretize a class whose level is not higher by 1
    lvlIsClassifierLvlMinusOne class' =
      maybe True
        (maybe False
          (\classifierExisting -> #level classifierExisting == #level class' + 1)
          . findClass
      ) (#classifier class')

    isLinked :: Name -> Bool
    isLinked className = any (\Link{source, target} -> source == className || target == className) links

    instantiatableOperations :: Class -> [Operation]
    instantiatableOperations = generateInstantiatableOperationsFinder classes

    instantiatesSomethingOrIsMetaClass :: Class -> Bool
    instantiatesSomethingOrIsMetaClass c@Class{classifier, name, slots} =
      isNothing classifier ||
      isLinked name ||
      not (null slots) ||
      not (null (instantiatableOperations c))

    validLink :: Link -> Bool
    validLink link@Link{source = linkSource, target = linkTarget} =
      maybe False (\Association{source = associationSource, target = associationTarget, lvlSource, lvlTarget} ->
          and [
              linkSource \/ associationSource,
              linkTarget \/ associationTarget,
              maybe False ((== lvlSource) . #level) (findClass linkSource),
              maybe False ((== lvlTarget) . #level) (findClass linkTarget)
            ]
        ) (findAssociationOfLink link)

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
      all (\Class{classifier = c, name} -> all ((== c) . getClassifier) (fromMaybe [] (parentDict !? name)) ) classes,
      all (\x -> valid (above x, map findClass (#parents x)) x) classes,
      all (\x -> valid (findClass (#source x), findClass (#target x)) x) associations,
      all associationMultiplicityNotViolated associations,
      all validLink links,
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
      eqBy x y (sort . #parents),
      eqBy x y #classifier,
      eqBy x y (sortOn #name . #attributes),
      eqBy x y (sortOn #name . #operations),
      eqBy x y (sortOn #name . #slots)
    ]

emptyClass :: Class
emptyClass = Class False 0 emptyName [] Nothing [] [] []

instance Validatable ([Class], [Maybe Class]) Class where
  valid (aboveThis, parentsClasses) Class{isAbstract, name = className, level = level', attributes = attributes', slots, operations, parents} = let

    getAttributeClass :: Name -> Maybe Class
    getAttributeClass x = find (elem x . map #name . #attributes) aboveThis

    getAttribute :: Name -> Maybe Attribute
    getAttribute x = find ((== x) . #name) . #attributes =<< getAttributeClass x

    allAttributesOfClassesAbove :: [Attribute]
    allAttributesOfClassesAbove = attributes' ++ concatMap #attributes aboveThis

    instantiatableAttributesHere = filter ((== level') . #level) (concatMap #attributes aboveThis) :: [Attribute]

    slotsNames = map #name slots :: [Name]

    in
    and [
        valid () className,
        valid () level',
        all (maybe False ((== level') . #level)) parentsClasses,
        allUnique parents,
        allUnique (map #name attributes'),
        allUnique (map #name slots),
        allUnique (map #name operations),
        allUnique (map (\Attribute{name, level} -> (name, level)) allAttributesOfClassesAbove),
        -- pretending that all attributes multiplicities are (1, Just 1)
        all (( `elem` slotsNames) . #name) instantiatableAttributesHere,
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
      -- enforcing this value of multiplicities because it is still not clear how to instantiate an attribute 0 times or >1 times
      multiplicity == Multiplicity (1, Just 1),
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
      maybe False (\x ->
          slotClassLvl == #level x && equalType (#dataType x) value
        ) slotAttribute

data Operation = Operation {
  level :: Int,
  name :: Name,
  dataType :: Type,
  isMonitored :: Bool,
  body :: String
} deriving (Show, Read)

emptyOperationBody :: Name -> String
emptyOperationBody (Name name') = "@Operation " ++ name' ++ "[monitor=false]():XCore::Integer&#99;  0&#10;end"

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
  multSource :: Multiplicity,
  multTarget :: Multiplicity,
  visibleSource :: Bool,
  visibleTarget :: Bool
} deriving (Show, Read, Eq)

emptyAssociation :: Association
emptyAssociation = Association emptyName emptyName emptyName 0 0 emptyMultiplicity emptyMultiplicity False False

instance Validatable (Maybe Class, Maybe Class) Association where
  valid (sourceClass, targetClass) Association{lvlSource, lvlTarget, multSource, multTarget, name} =
    and [
      valid () name,
      maybe False ((> lvlSource) . #level) sourceClass,
      maybe False ((> lvlTarget) . #level) targetClass,
      valid () multSource,
      valid () multTarget
      ]

data Link = Link {
  name :: Name,
  source :: Name,
  target :: Name
} deriving (Show, Read, Eq, Ord)

emptyLink :: Link
emptyLink = Link emptyName emptyName emptyName

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

typeSpace :: [Type]
typeSpace = [minBound .. maxBound]

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
