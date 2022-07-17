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
  valid,
  getTypeName,
  sameTypeAs,
  isUnassigned,
  relativeToEur,
  currencySymbol
) where

import Data.List.UniqueStrict (allUnique)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (find, sort, group)
import Data.Ix (inRange)

import GHC.OverloadedLabels
import GHC.Records

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

class Validatable c a where
  valid :: c -> a -> Bool

newtype Name = Name String deriving (Eq, Ord)

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

instance Validatable () MLM where
  valid () (MLM {name = projectName, classes, associations, links}) = let
    -- navigation
    getClass className = find ((== className) . #name) classes
    getAssociation link' = find ((== #association link') . #name) associations

    -- x inheritsFrom y
    inheritsFrom :: Name -> Name -> Bool
    inheritsFrom x y = maybe False
      (any (\parent -> parent == y || parent `inheritsFrom` y) . #parents)
      (getClass x)

    -- x concretizes y
    concretizes x y = maybe False
      (maybe False (\classifier' -> classifier' == y || classifier' `concretizes` y) . #classifier)
      (getClass x)

    -- x concretizesOrInheritsFrom y:
    (<--) x0 y = let x = getClass x0 in or [
      x0 `inheritsFrom` y,
      x0 `concretizes` y,
      maybe False (maybe False (<-- y) . #classifier) x,
      maybe False (any (<-- y) . #parents) x
      ]

    -- whether an association multiplicity is violated:
    linksOf x = filter ((#name x ==) . #association) links
    sourcesOf = map #source . linksOf
    targetsOf = map #target . linksOf
    allInRange (Multiplicity (a,b)) = all (inRange (a,b) . length) . group . sort
    multNotViolated x =
      allInRange (multSourceToTarget x) (sourcesOf x) &&
      allInRange (multTargetToSource x) (targetsOf x)

    -- ...
    lvlIsClassifierLvlMinusOne class' =
      maybe True (\classifierMentioned ->
        maybe False (\classifierExisting ->
          (#level classifierExisting ==
          #level class' + 1)
        ) (getClass classifierMentioned)
      ) (#classifier class')

    -- whether source of link concretizes or inherits from source of association of that link and
    -- whether target of link concretizes or inherits from target of association of that link
    checkSourceAndTarget x = maybe False (\asso ->
        #source x <-- #source asso && #target x <-- #target asso
      ) (getAssociation x)

    -- determine scope:
    scope :: Class -> [Class]
    scope x = filter ((#name x <--) . #name) classes

    in and [
      not (null classes),
      valid () projectName,
      allUnique (map #name classes),
      allUnique (map #name associations),
      all multNotViolated associations,
      all ((\x -> not (x <-- x)) . #name) classes,
      all lvlIsClassifierLvlMinusOne classes,
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

instance Validatable ([Class], [Maybe Class]) Class where
  valid (classScope, parentsClasses) (Class {name = className, level = level', attributes = attributes', operations, slots, parents}) = let
    getAttributeClass x = find (elem x . map #name . #attributes) classScope
    -- getAttribute x = maybe Nothing (find ((== attribute) . name) . attributes) (getAttributeClass x)
    getAttribute x = ((find ((== x) . #name) . #attributes) =<< getAttributeClass x)
    in
    and [
        valid () className,
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
  type' :: Type,
  multiplicity :: Multiplicity
} deriving (Eq, Show)

instance Validatable Level Attribute where
  valid classLevel (Attribute {multiplicity, level = attributeLevel, type', name}) = and [
      valid () attributeLevel,
      valid () name,
      valid () multiplicity,
      isUnassigned type',
      classLevel > attributeLevel
    ]

data Slot = Slot {
  attribute :: Name,
  value :: Type
} deriving (Eq, Show)

instance Validatable (Maybe Attribute, Level) Slot where
  valid (slotAttribute, slotClassLvl) (Slot {value}) = let
    in
      maybe False ((slotClassLvl ==) . #level) slotAttribute
      &&
      not (isUnassigned value)

data Operation = Operation {
  level :: Int,
  name :: Name,
  type' :: Type,
  isMonitored :: Bool,
  body :: OperationBody
} deriving (Eq, Show)

instance Validatable Level Operation where
  valid operationClassLvl (Operation {level = level', type'}) =
    operationClassLvl > level' && isUnassigned type'

data OperationBody = OperationBody {
  placeholder1 :: String,
  placeholder2 :: String
} deriving (Eq, Show)

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

newtype Multiplicity = Multiplicity (Int, Int) deriving (Eq, Show)

instance Validatable () Multiplicity where
  valid () (Multiplicity (lower, upper)) =
    lower >= 0 &&
    upper <= upper || upper == -1

type Level = Int

instance Validatable () Level where
  valid () level = level >= 0

data Type =
  Boolean (Maybe Bool) |
  Integer (Maybe Integer) |
  Float (Maybe Float) |
  String (Maybe String) |
  Element (Maybe ()) |
  MonetaryValue (Maybe (String, String)) |
  Date (Maybe (Int, Int, Int)) |
  Currency (Maybe XModelerCurrency) |
  Complex (Maybe String) |
  AuxiliaryClass (Maybe String)
  deriving (Eq, Show)

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
    AuxiliaryClass Nothing
    ]

getTypeName :: Type -> String
getTypeName = head . splitOn " " . show

sameTypeAs :: Type -> Type -> Bool
sameTypeAs x y = getTypeName x == getTypeName y

data XModelerCurrency = USD | EUR | GBP | AUD | NZD deriving (Eq, Show)

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
