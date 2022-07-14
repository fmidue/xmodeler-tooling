{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
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
import Data.List (find)

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
  valid () mlm@(MLM {name = name', classes, associations, links}) = and [
        not (null classes),
        valid () name',
        allUnique (map (name :: Class -> Name) classes),
        allUnique (map (name :: Association -> Name) associations),
        all (valid mlm) classes,
        all (valid mlm) associations,
        all (valid mlm) links
      ]

data Class = Class {
  isAbstract :: Bool,
  level :: Level,
  name :: Name,
  parents :: [Name],
  isOf :: Maybe Name,
  attributes :: [Attribute],
  operations :: [Operation],
  slots :: [Slot]
} deriving (Eq, Show)

getClass :: MLM -> Name -> Maybe Class
getClass (MLM {classes}) x =
  find ((== x) . (name :: Class -> Name)) classes

inheritsFrom :: MLM -> Name -> Name -> Bool
inheritsFrom mlm w z =
  maybe False
    (\x -> z `elem` parents x || any (\y -> inheritsFrom mlm y z) (parents x))
  (getClass mlm w)

instantiates :: MLM -> Name -> Name -> Bool
instantiates mlm x z =
  maybe False
    (maybe False (\y -> y == z || concretizes mlm y z) . isOf)
  (getClass mlm x)

concretizes :: MLM -> Name -> Name -> Bool
concretizes mlm w z = let
  x = getClass mlm w
  in
    inheritsFrom mlm w z ||
    instantiates mlm w z ||
    maybe False (maybe False (\y -> concretizes mlm y z) . isOf) x ||
    maybe False (any (\y -> concretizes mlm y z) . parents) x

instance Validatable MLM Class where
  valid mlm (Class {name = className, level = level' , parents, isOf, attributes, operations, slots}) = and [
        valid () className,
        all (maybe False ((== level') . (level :: Class -> Level)) . getClass mlm) parents,
        allUnique parents,
        not (concretizes mlm className className),
        level' > 0 || (null attributes && null operations),
        all (valid level') attributes,
        all (valid (mlm, className)) operations,
        all (valid (mlm, className)) slots,
        maybe True (maybe True ((== level' + 1) . (level :: Class -> Level)) . getClass mlm) isOf
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

instance Validatable (MLM, Name) Slot where
  valid (mlm@MLM {classes}, slotClass) (Slot {attribute, value}) = let
    findAttributeClass :: Maybe Class
    findAttributeClass  = find (elem attribute . map (name :: Attribute -> Name) . attributes) (classes :: [Class])
--    attribute' = maybe Nothing (find ((== attribute) . name) . attributes) findAttributeClass
    attribute' = ((find ((== attribute) . (name :: Attribute -> Name)) . attributes) =<< findAttributeClass)
    in
      maybe False (concretizes mlm slotClass . (name :: Class -> Name)) findAttributeClass &&
      maybe False ((\x -> maybe False ((== x) . (level :: Attribute -> Level)) attribute') . (level :: Class -> Level)) (getClass mlm slotClass) &&
      not (isUnassigned value)

data Operation = Operation {
  level :: Int,
  name :: Name,
  type' :: Type,
  isMonitored :: Bool,
  body :: OperationBody
} deriving (Eq, Show)

instance Validatable (MLM, Name) Operation where
  valid (mlm, operationClass) (Operation {level = level', type'}) =
    maybe False ((> level') . (level :: Class -> Level)) (getClass mlm operationClass) &&
    isUnassigned type'

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

getAssociation :: MLM -> Name -> Maybe Association
getAssociation (MLM {associations}) x =
  find ((== x) . (name :: Association -> Name)) associations

instance Validatable MLM Association where
  valid mlm (Association {source, target, lvlSource, lvlTarget, multTargetToSource, multSourceToTarget, name}) =
    and [
      valid () name,
      maybe False ((> lvlSource) . (level :: Class -> Level)) (getClass mlm source),
      maybe False ((> lvlTarget) . (level :: Class -> Level)) (getClass mlm target),
      valid () multTargetToSource,
      valid () multSourceToTarget
      ]

data Link = Link {
  association :: Name,
  source :: Name,
  target :: Name
} deriving (Eq, Show)

instance Validatable MLM Link where
  valid mlm (Link {association, source = linkSource , target = linkTarget}) = let
    association' = getAssociation mlm association
    check condition = maybe False condition association'
    in
    and [
      check (concretizes mlm linkSource . (source :: Association -> Name)),
      check (concretizes mlm linkTarget . (target :: Association -> Name)),
      check (\x -> maybe False ((== lvlSource x) . (level :: Class -> Level)) (getClass mlm linkSource)),
      check (\x -> maybe False ((== lvlTarget x) . (level :: Class -> Level)) (getClass mlm linkTarget))
    ]

data Multiplicity = Multiplicity {
  lower :: Int,
  upper :: Int
} deriving (Eq, Show)

instance Validatable () Multiplicity where
  valid () (Multiplicity {lower, upper}) =
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
