{-# LANGUAGE QuasiQuotes #-}

module Modelling.Types (
  MetaClass (..),
  Instance (..),
  ChangeParent (..),
  Attribute (..),
  Operation (..),
  ChangeSlotValue (..),
  Association (..),
  Link (..),
  MLM (..),
  Type (..),
  XModelerCurrency (..),
  getType,
  getValue
  ) where

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

data MetaClass = MetaClass {
  mAbstract :: Bool,
  mLevel :: Int,
  mName :: String
  }

data Instance = Instance {
  iAbstract :: Bool,
  iName :: String,
  iOf :: String
  }

data ChangeParent = ChangeParent {
  pClass :: String,
  pNew :: [String]
  }

data Attribute = Attribute {
  aClass :: String,
  aLevel :: Int,
  aName :: String,
  aType :: Type,
  aMultiplicityMin :: Int,
  aMultiplicityMax :: Int
  }

data Operation = Operation {
  oClass :: String,
  oLevel :: Int,
  oName :: String,
  oType :: Type,
  oBody :: String
  }

data ChangeSlotValue = ChangeSlotValue {
  vClass :: String,
  vName :: String,
  vValueToBeParsed :: Type
  }

data Association = Association {
  sFwName :: String,
  sSource :: String,
  sTarget :: String,
  sInstLevelSource :: Int,
  sInstLevelTarget :: Int,
  sMultTargetToSourceMin :: Int,
  sMultTargetToSourceMax :: Int,
  sMultSourceToTargetMin :: Int,
  sMultSourceToTargetMax :: Int,
  sSourceVisibleFromTarget :: Bool,
  sTargetVisibleFromSource :: Bool
  }

data Link = Link {
  lName :: String,
  lClassSource :: String,
  lClassTarget :: String
  }

data MLM = MLM {
  mlmName :: String,
  mlmMetaClasses :: [MetaClass],
  mlmInstances :: [Instance],
  mlmChangeParents :: [ChangeParent],
  mlmAttributes :: [Attribute],
  mlmOperations :: [Operation],
  mlmChangeSlotValues :: [ChangeSlotValue],
  mlmAssociations :: [Association],
  mlmLinks :: [Link]
  }