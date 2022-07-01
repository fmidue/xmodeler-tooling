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
  relativeToEur,
  currencySymbol
) where

import Data.List.UniqueStrict
import Data.Char (isDigit)

class Valid c a where
  valid :: c -> a -> Bool

data MLM = MLM {
  projectName :: String,
  classes :: [Class],
  associations :: [Association],
  links :: [Link]
}

instance Valid () MLM where
  valid () (MLM {projectName, classes, associations, links}) = let
    mlmClassesNames = map cName classes
    mlmAssociationsNames = map sName associations
    validChar1 = flip elem (['a'..'z'] ++ ['A'..'Z'])
    validCharN char = validChar1 char || isDigit char
    in
      and [
        not (null projectName),
        validChar1 (head projectName),
        all validCharN (tail projectName),
        all (valid ()) classes,
        all (valid classes) associations,
        all (valid associations) links,
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
} deriving Eq

instance Valid () Class where
  valid () (Class {cLevel = level, parents, cIsOf, attributes, operations, slots}) = and [
    all ((== level) . cLevel) parents,
    all (valid level) attributes,
    all (valid level) operations,
    level > 0 || (null attributes && null operations),
    all (valid cIsOf) slots,
    case cIsOf of
       Nothing -> True
       Just x -> cLevel x == level + 1
    ]

data Attribute = Attribute {
  tLevel :: Level,
  tName :: String,
  tType :: Type,
  multiplicity :: Multiplicity
  } deriving Eq

instance Valid Level Attribute where
  valid classLevel (Attribute {multiplicity, tLevel}) =
    valid () multiplicity &&
    tLevel < classLevel

data Slot = Slot {
  attribute :: Attribute,
  value :: Value
  } deriving Eq

instance Valid (Maybe Class) Slot where
  valid Nothing _ = False
  valid (Just class') slot@(Slot {attribute}) =
    attribute `elem` attributes class' || valid (cIsOf class') slot

data Operation = Operation {
  oLevel :: Int,
  oName :: String,
  oType :: Type,
  isMonitored :: Bool,
  body :: String
  } deriving Eq

instance Valid Int Operation where
  valid classLevel (Operation {oLevel}) = oLevel < classLevel

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
  } deriving Eq

instance Valid [Class] Association where
  valid mlmClasses (Association {sSource, sTarget,
    lvlSource, lvlTarget, multTargetToSource, multSourceToTarget}) = and [
    sSource `elem` mlmClasses,
    sTarget `elem` mlmClasses,
    lvlSource < cLevel sSource,
    lvlTarget < cLevel sTarget,
    all (valid ()) [lvlSource, lvlTarget],
    all (valid ()) [multTargetToSource, multSourceToTarget]
    -- might add restrictions to naming, later. For example, you cannot start the name with a digit.
    ]

data Link = Link {
  lIsOf :: Association,
  lSource :: Class,
  lTarget :: Class
  } deriving Eq

instance Valid [Association] Link where
  valid mlmAssociations (Link {lIsOf, lSource, lTarget}) = and [
    lIsOf `elem` mlmAssociations,
    lSource == sSource lIsOf,
    lTarget == sTarget lIsOf,
    cLevel lSource == lvlSource lIsOf,
    cLevel lTarget == lvlTarget lIsOf
    ]

data Multiplicity = Multiplicity {
  lower :: Int,
  upper :: Int
} deriving Eq

instance Valid () Multiplicity where
  valid () (Multiplicity {lower, upper}) =
    lower >= 0 &&
    upper <= upper || upper == -1

type Level = Int

instance Valid () Level where
  valid () level = level >= 0

data Type = Boolean | Integer | Float | String | Element | MonetaryValue | Date | Currency | Complex | AuxiliaryClass deriving (Show, Eq)

data Value = B Bool | I Int | F Float | S String | E () | M String String | D Int Int Int | C XModelerCurrency | X String | A String deriving Eq

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
