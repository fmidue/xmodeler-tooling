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
  OperationBody (..),
  Level,
  Validatable,
  Type (..),
  XModelerCurrency (..),
  valid,
  getTypeName,
  sameTypeAs,
  isUnassigned,
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
  emptyMultiplicity,
  emptyType
) where

import Data.List.UniqueStrict (allUnique)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (find, sort, group)
import Data.Ix (inRange)
import Data.Maybe (isJust, maybeToList)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))

noCycles :: Eq a => [(a, a)] -> Bool
noCycles [] = True
noCycles list = let peeled = filter (isJust . flip lookup list . snd) list in
  (length peeled < length list) && noCycles peeled

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

class Validatable context a where
  valid :: context -> a -> Bool

newtype Name = Name String deriving (Eq, Ord)

emptyName :: Name
emptyName = Name ""

instance Validatable () Name where
  valid () (Name name) = let
    validChar1 = flip elem (['a'..'z'] ++ ['A'..'Z'])
    validCharN char = validChar1 char || isDigit char || char == '_'
    in
      all ($ name)
        [not . null, validChar1 . head, all validCharN . tail]

instance Show Name where
  show (Name name) = name

data MLM = MLM {
  name :: Name,
  classes :: [Class],
  associations :: [Association],
  links :: [Link]
} deriving Show

emptyMLM :: MLM
emptyMLM = MLM emptyName [] [] []

instance Validatable () MLM where
  valid () (MLM {name = projectName, classes, associations, links}) = let
    allClassesNames = map #name classes :: [Name]

    -- navigation
    getClass className = find ((== className) . #name) classes
    getAssociation link' = find ((== #association link') . #name) associations

    parentDictList :: [(Name, [Name])]
    parentDictList = map (\(Class {name, parents}) -> (name, parents)) classes

    classifierDictList :: [(Name, [Name])]
    classifierDictList = filter (not . null . snd) $
      map (\(Class {name, classifier}) -> (name, maybeToList classifier))
      classes

    unlistDict :: [(a, [b])] -> [(a, b)]
    unlistDict = concatMap (\(x, xs) -> map (x, ) xs)

    parentDict :: [(Name, Name)]
    parentDict = unlistDict parentDictList

    classifierDict :: [(Name, Name)]
    classifierDict = unlistDict classifierDictList

    dict :: [(Name, Name)]
    dict = parentDict ++ classifierDict

    getMetaClass :: Name -> Maybe Name
    getMetaClass x = getMetaClass =<< lookup x classifierDict

    sameMetaClass :: (Name, Name) -> Bool
    sameMetaClass (x, y) = getMetaClass x == getMetaClass y

    -- whether x concretizes or inherits from z
    (\/) :: Name -> Name -> Bool
    (\/) x z = maybe False (\y -> y==z || y \/ z) $ lookup x dict

    -- whether an association multiplicity is violated:
    linksOf :: Association -> [Link]
    linksOf x = filter ((#name x ==) . #association) links
    sourcesOf :: Association -> [Name]
    sourcesOf = map #source . linksOf
    targetsOf :: Association -> [Name]
    targetsOf = map #target . linksOf
    inRangeOfMult :: Multiplicity -> Int -> Bool
    inRangeOfMult (Multiplicity (a, Nothing)) x = x >= a
    inRangeOfMult (Multiplicity (a, Just b)) x = inRange (a, b) x
    allInRangeOfMult :: Multiplicity -> [Name] -> Bool
    allInRangeOfMult mult = all (inRangeOfMult mult . length) . group . sort

    multNotViolated :: Association -> Bool
    multNotViolated x@(Association {multSourceToTarget, multTargetToSource}) =
      allInRangeOfMult multSourceToTarget (sourcesOf x) &&
      allInRangeOfMult multTargetToSource (targetsOf x)

    -- whether a class concretizes a class whose level is not higher by 1
    lvlIsClassifierLvlMinusOne class' =
      maybe True
        (maybe False
          (\classifierExisting -> #level classifierExisting == #level class' + 1)
          . getClass
      ) (#classifier class')

    isLinked :: Name -> Bool
    isLinked className = any (\(Link {source, target}) -> source == className || target == className) links

    -- whether source of link concretizes or inherits from source of association of that link and
    -- whether target of link concretizes or inherits from target of association of that link
    checkSourceAndTarget x = maybe False (\asso ->
        #source x \/ #source asso && #target x \/ #target asso
      ) (getAssociation x)

    -- determine scope:
    scope :: Class -> [Class]
    scope x = filter ((#name x \/) . #name) classes

    in and [
      not (null classes),
      valid () projectName,
      allUnique allClassesNames,
      allUnique (map #name associations),
      noCycles dict,
      all lvlIsClassifierLvlMinusOne classes,
      all (\Class {name, operations, attributes} ->
        not (null attributes) || not (null operations) || isLinked name)
        (filter (isJust . #classifier) classes),
      all sameMetaClass parentDict,
      all (\x -> valid (scope x, map getClass (#parents x)) x) classes,
      all (\x -> valid (getClass (#source x), getClass (#target x)) x) associations,
      all multNotViolated associations,
      all checkSourceAndTarget links,
      all (\x -> valid
          (getClass (#source x),getClass (#target x))
          (getAssociation x)
        ) links
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
} deriving (Eq, Show)

emptyClass :: Class
emptyClass = Class False 0 emptyName [] Nothing [] [] []

instance Validatable ([Class], [Maybe Class]) Class where
  valid (classScope, parentsClasses) (Class {name = className, level = level', attributes = attributes', operations, slots, parents}) = let
    getAttributeClass x = find (elem x . map #name . #attributes) classScope
    getAttribute x = ((find ((== x) . #name) . #attributes) =<< getAttributeClass x)
    in
    and [
        valid () className,
        valid () level',
        all (maybe False ((== level') . #level)) parentsClasses,
        allUnique parents,
        level' > 0 || (null attributes' && null operations),
        all (valid level') attributes',
        all (valid level') operations,
        all (\x -> valid (getAttribute (#attribute x) , level') x) slots
        ]

data Attribute = Attribute {
  level :: Level,
  name :: Name,
  dataType :: Type,
  multiplicity :: Multiplicity
} deriving (Eq, Show)

emptyAttribute :: Attribute
emptyAttribute = Attribute 0 emptyName emptyType emptyMultiplicity

instance Validatable Level Attribute where
  valid classLevel (Attribute {multiplicity, level = attributeLevel, dataType, name}) = and [
      valid () attributeLevel,
      valid () name,
      valid () multiplicity,
      isUnassigned dataType,
      classLevel > attributeLevel
    ]

data Slot = Slot {
  attribute :: Name,
  value :: Type
} deriving (Eq, Show)

emptySlot :: Slot
emptySlot = Slot emptyName emptyType

instance Validatable (Maybe Attribute, Level) Slot where
  valid (slotAttribute, slotClassLvl) (Slot {value}) =
      maybe False ((slotClassLvl ==) . #level) slotAttribute
      &&
      not (isUnassigned value)

data Operation = Operation {
  level :: Int,
  name :: Name,
  dataType :: Type,
  isMonitored :: Bool,
  body :: OperationBody
} deriving (Eq, Show)

emptyOperation :: Operation
emptyOperation = Operation 0 emptyName emptyType False emptyOperationBody

instance Validatable Level Operation where
  valid operationClassLvl (Operation {level , dataType}) =
    operationClassLvl > level && isUnassigned dataType

data OperationBody = OperationBody {
  placeholder1 :: String,
  placeholder2 :: String
} deriving (Eq, Show)

emptyOperationBody :: OperationBody
emptyOperationBody = OperationBody "" ""

instance Validatable mlm OperationBody where
  valid _ _ = True --placeholder

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
} deriving (Eq, Show)

emptyAssociation :: Association
emptyAssociation = Association emptyName emptyName emptyName 0 0 emptyMultiplicity emptyMultiplicity False False

instance Validatable (Maybe Class, Maybe Class) Association where
  valid (sourceClass, targetClass) (Association {lvlSource, lvlTarget, multTargetToSource, multSourceToTarget, name}) =
    and [
      valid () name,
      maybe False ((> lvlSource) . #level) sourceClass,
      maybe False ((> lvlTarget) . #level) targetClass,
      valid () multTargetToSource,
      valid () multSourceToTarget
      ]

data Link = Link {
  association :: Name,
  source :: Name,
  target :: Name
} deriving (Eq, Show)

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

newtype Multiplicity = Multiplicity (Int, Maybe Int) deriving (Eq, Show)

emptyMultiplicity :: Multiplicity
emptyMultiplicity = Multiplicity (0, Nothing)

instance Validatable () Multiplicity where
  valid () (Multiplicity (lower, Nothing)) = lower >= 0
  valid () (Multiplicity (lower, Just upper)) = lower >= 0 && lower <= upper

type Level = Int

instance Validatable () Level where
  valid () level = level >= 0

data Type =
  Boolean (Maybe Bool) |
  Integer (Maybe Integer) |
  Float (Maybe Float) |
  String (Maybe String) |
  Element (Maybe ()) |
  MonetaryValue (Maybe (Float, String)) |
  Date (Maybe (Int, Int, Int)) |
  Currency (Maybe XModelerCurrency) |
  Complex (Maybe String) |
  AuxiliaryClass (Maybe String) |
  Null
  deriving (Eq, Show, Read)

emptyType :: Type
emptyType = Boolean Nothing

isUnassigned :: Type -> Bool
isUnassigned = flip elem [
    Boolean Nothing,
    Integer Nothing,
    Float Nothing,
    String Nothing,
    Element Nothing,
    MonetaryValue Nothing,
    Date Nothing,
    Currency Nothing,
    Complex Nothing,
    AuxiliaryClass Nothing,
    Null
    ]

getTypeName :: Type -> String
getTypeName = head . splitOn " " . show

sameTypeAs :: Type -> Type -> Bool
sameTypeAs Null _ = True
sameTypeAs _ Null = True
sameTypeAs x y = getTypeName x == getTypeName y

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
