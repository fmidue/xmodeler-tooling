{-# LANGUAGE NamedFieldPuns #-}
module Modelling.MLM.Types(
  MLM (..),
  Link (..),
  Association (..),
  Slot (..),
  Class (..),
  Operation (..),
  Attribute (..),
  Multiplicity (..),
  Value (..),
  Type (..),
  Level,
  Validatable,
  valid,
  relativeToEur,
  currencySymbol
) where

import Data.List.UniqueStrict
import Data.Char (isDigit)

class Validatable a where
  valid :: a -> Bool


data MLM = MLM {
  projectName :: String,
  classes :: [Class],
  associations :: [Association],
  links :: [Link]
} deriving Show

instance Validatable MLM where
  valid (MLM {projectName, classes, associations, links}) = let
    mlmClassesNames = map cName classes
    mlmAssociationsNames = map sName associations
    validChar1 = flip elem (['a'..'z'] ++ ['A'..'Z'])
    validCharN char = validChar1 char || isDigit char || char == '_'
    in
      and [
        not (null projectName),
        validChar1 (head projectName),
        all validCharN (tail projectName),
        all valid classes,
        all valid associations,
        all valid links,
        all allUnique [mlmClassesNames, mlmAssociationsNames]
      ]

data Class = Class {
  isAbstract :: Bool,
  cLevel :: Level,
  cName :: String,
  parents :: [Class],
  cIsOf :: Maybe Class,
  attributes :: [Attribute],
  operations :: [Operation],
  slots :: [Slot]
} deriving (Eq, Show)

instance Validatable Class where
  valid (Class {cLevel = level, parents, cIsOf, attributes, operations, slots}) = and [
    all ((== level) . cLevel) parents,
    all valid attributes,
    all valid operations,
    level > 0 || (null attributes && null operations),
    all valid slots,
    case cIsOf of
       Nothing -> True
       Just x -> cLevel x == level + 1
    ]

data Attribute = Attribute {
  tLevel :: Level,
  tName :: String,
  tType :: Type,
  tClass :: Class,
  multiplicity :: Multiplicity
} deriving (Eq, Show)

instance Validatable Attribute where
  valid (Attribute {multiplicity, tLevel, tClass}) =
    valid multiplicity &&
    tLevel < cLevel tClass

data Slot = Slot {
  attribute :: Attribute,
  value :: Value,
  slClass :: Class
} deriving (Eq, Show)

instance Validatable Slot where
  valid slot@(Slot {attribute, slClass}) =
    attribute `elem` attributes slClass ||
    case cIsOf slClass of
      Nothing -> False
      Just c ->  valid (slot {slClass = c})

data Operation = Operation {
  oLevel :: Int,
  oName :: String,
  oType :: Type,
  isMonitored :: Bool,
  oClass :: Class,
  body :: String
} deriving (Eq, Show)

instance Validatable Operation where
  valid (Operation {oLevel, oClass}) =
    cLevel oClass > oLevel

data Association = Association {
  sName :: String,
  sSource :: Class,
  sTarget :: Class,
  lvlSource :: Level,
  lvlTarget :: Level,
  multTargetToSource :: Multiplicity,
  multSourceToTarget :: Multiplicity,
  sourceVisibleFromTarget :: Bool,
  targetVisibleFromSource :: Bool
} deriving (Eq, Show)

instance Validatable Association where
  valid (Association {sSource, sTarget, lvlSource, lvlTarget, multTargetToSource, multSourceToTarget}) = and [
    lvlSource < cLevel sSource,
    lvlTarget < cLevel sTarget,
    all valid [lvlSource, lvlTarget],
    all valid [multTargetToSource, multSourceToTarget]
    -- might add restrictions to naming, later. For example, you cannot start the name with a digit.
    ]

data Link = Link {
  lIsOf :: Association,
  lSource :: Class,
  lTarget :: Class,
  lClass :: Class
} deriving (Eq, Show)

instance Validatable Link where
  valid (Link {lIsOf, lSource, lTarget}) = and [
    lSource == sSource lIsOf,
    lTarget == sTarget lIsOf,
    cLevel lSource == lvlSource lIsOf,
    cLevel lTarget == lvlTarget lIsOf
    ]

data Multiplicity = Multiplicity {
  lower :: Int,
  upper :: Int
} deriving (Eq, Show)

instance Validatable Multiplicity where
  valid (Multiplicity {lower, upper}) =
    lower >= 0 &&
    upper <= upper || upper == -1

type Level = Int

instance Validatable Level where
  valid level = level >= 0

data Type = Boolean | Integer | Float | String | Element | MonetaryValue | Date | Currency | Complex | AuxiliaryClass deriving (Show, Eq)

data Value = B Bool | I Int | F Float | S String | E () | M String String | D Int Int Int | C XModelerCurrency | X String | A String deriving (Eq, Show)

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
