-- {-# LANGUAGE QuasiQuotes #-}

-- remember to export these properly when done testing:
module Modelling.MLM.Types where

import Data.Maybe (isNothing, fromJust)

data MLM = MLM {
  classes :: [Class],
  associations :: [Association],
  links :: [Link]
}

class ValidClass a where
  valid :: a -> Bool

{-
I am aware of
"In probably more than 90% of occurrences, use of fromJust is a mistake (i.e., is bad coding)."
The following just for learning purposes and could eventually probably be avoided by pattern-matching or something:
-}
instance ValidClass Class where
  valid c =  all ((== cLevel c) . cLevel) (parents c)
          && ( isNothing (isOf c) || cLevel (fromJust (isOf c)) == cLevel c + 1)

data Class = Class {
  isAbstract :: Bool,
  cLevel :: Int,
  cName :: String,
  parents :: [Class],
  isOf :: Maybe Class,
  attributes :: [Attribute],
  operations :: [Operation]
}

data Attribute = Attribute {
  tLevel :: Int,
  tName :: String,
  tType :: Type,
  multiplicity :: Multiplicity,
  value :: Maybe Value
  }

data Operation = Operation {
  oLevel :: Int,
  oName :: String,
  oType :: Type,
  body :: String
  }

data Association = Association {
  sName :: String,
  sSource :: Class,
  sTarget :: Class,
  lvlSource :: Int,
  lvlTarget :: Int,
  multTargetToSource :: Multiplicity,
  multSourceToTarget :: Multiplicity,
  sourceVisibleFromTarget :: Bool,
  targetVisibleFromSource :: Bool
  }

data Link = Link {
  lName :: String,
  lSource :: Class,
  lTarget :: Class
  }

data Multiplicity = Multiplicity {
  lower :: Int,
  upper :: Int
}

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