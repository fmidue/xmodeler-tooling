-- {-# LANGUAGE QuasiQuotes #-}
-- remember to export these properly when done testing:
module Modelling.MLM.Types where

import Data.List.UniqueStrict

class Valid c a where
  valid :: c -> a -> Bool

data MLM = MLM {
  classes :: [Class],
  associations :: [Association],
  links :: [Link]
}

instance Valid () MLM where
  valid () mlm = let
    mlmClasses = classes mlm
    mlmClassesNames = map cName mlmClasses
    mlmAssociations = associations mlm
    mlmAssociationsNames = map sName mlmAssociations
    mlmLinks = links mlm
    in
      and [
        all (valid ()) mlmClasses,
        all (valid mlmClasses) mlmAssociations,
        all (valid mlmAssociations) mlmLinks,
        all allUnique [mlmClassesNames, mlmAssociationsNames] -- I could not find a function to check for duplicates
      ]

data Class = Class {
  isAbstract :: Bool,
  cLevel :: Level,
  cName :: String,
  parents :: [Class],
  cIsOf :: Maybe Class,
  attributes :: [Attribute],
  operations :: [Operation]
} deriving Eq

instance Valid () Class where
  valid () (Class _ lvl _ prnts isOf attr _) =
    all (valid ()) attr &&
    all ((== lvl) . cLevel) prnts &&
    case isOf of
      Nothing -> True
      Just x -> cLevel x == lvl + 1

data Attribute = Attribute {
  tLevel :: Level,
  tName :: String,
  tType :: Type,
  multiplicity :: Multiplicity,
  value :: Maybe Value
  } deriving Eq

instance Valid () Attribute where
  valid () = valid () . multiplicity

data Operation = Operation {
  oLevel :: Int,
  oName :: String,
  oType :: Type,
  body :: String
  } deriving Eq

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
  valid mlmClasses (Association _ source target
    lvlS lvlT multST multTS _ _) = and [
    source `elem` mlmClasses,
    target `elem` mlmClasses,
    lvlS < cLevel source,
    lvlT < cLevel target,
    all (valid ()) [lvlS, lvlT],
    all (valid ()) [multST, multTS]
    -- might add restrictions to naming, later. For example, you cannot start the name with a digit.
    ]

data Link = Link {
  lIsOf :: Association,
  lSource :: Class,
  lTarget :: Class
  } deriving Eq

instance Valid [Association] Link where
  valid mlmAssociations (Link isOf source target) = and [
    isOf `elem` mlmAssociations,
    source == sSource isOf,
    target == sTarget isOf,
    cLevel source == lvlSource isOf,
    cLevel target == lvlTarget isOf
    ]

data Multiplicity = Multiplicity {
  lower :: Int,
  upper :: Int
} deriving Eq

instance Valid () Multiplicity where
  valid () (Multiplicity lowerBound upperBound) =
    lowerBound >= 0 &&
    lowerBound <= upperBound || upperBound == -1

type Level = Int

instance Valid () Level where
  valid () level = level >= 0

type Value = String
type Type = String

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