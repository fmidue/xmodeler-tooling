{-# LANGUAGE NamedFieldPuns #-}
-- remember to export these properly when done testing:
module Modelling.MLM.Types where

import Data.List.UniqueStrict
import Data.Char (isDigit)
import Data.Set (fromList)

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

data Slot = Slot Attribute Value deriving Eq

instance Valid (Maybe Class) Slot where
  valid Nothing _ = False
  valid (Just class') slot@(Slot att _) =
    att `elem` attributes class' || valid (cIsOf class') slot

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

type Value = String

type Object = (String, (Int, Int))

instance Valid [String] [Object] where
  valid mlmClassesNames objects =
    fromList (map fst objects) == fromList mlmClassesNames

-- I think these are going to be useful later:
{-


import Data.Char (ord, toLower)
import Data.String.Interpolate (i)
import Data.List.Split (splitOn)
import Data.List (intercalate)

data XModelerCurrency = USD | EUR | GBP | AUD | NZD deriving Show


relativeToEur :: XModelerCurrency -> Float
relativeToEur USD = 0.9502091
relativeToEur EUR = 1.0
relativeToEur GBP = 1.165868
relativeToEur AUD = 0.65582377
relativeToEur NZD = 0.5976929

data Type =
  XCore_Boolean (Maybe Bool) |
  XCore_Integer (Maybe Int) |
  XCore_Float (Maybe Float) |
  XCore_String (Maybe String) |
  XCore_Element (Maybe ()) |
  Auxiliary_MonetaryValue (Maybe (String, String)) |
  Auxiliary_Date (Maybe (Int, Int, Int)) |
  Auxiliary_Currency (Maybe XModelerCurrency) |
  Auxiliary_Complex (Maybe String) |
  Auxiliary_AuxiliaryClass (Maybe String)
  deriving Show

getType :: Type -> String
getType =  intercalate "::" . splitOn "_" . head . splitOn " " . show

getValue :: Type -> String
getValue t = case t of
  Auxiliary_MonetaryValue (Just (unit, value)) -> [i|Auxiliary::MonetaryValue(#{value}, Auxiliary::Currency(&quot;#{unit}&quot;, &quot;#{value}&quot;, 1.0))|]
  Auxiliary_Date (Just (year, month, day)) -> [i|Auxiliary::Date::createDate(#{year}, #{month}, #{day})|]
  Auxiliary_Currency (Just x) -> [i|Auxiliary::Currency(&quot;$&quot;, &quot;#{map toLower (show x)}&quot;, #{relativeToEur x})|]
  Auxiliary_Complex (Just _) -> "null"
  Auxiliary_AuxiliaryClass (Just _) -> "null"
  XCore_Boolean (Just x) -> map toLower (show x)
  XCore_Integer (Just x) -> show x
  XCore_Float (Just x) -> show x
  XCore_String (Just x) -> [i|#{map ord x}.asString()|]
  XCore_Element (Just _) -> "null"
  _ -> error "Invalid Type and/or Value!"
-}