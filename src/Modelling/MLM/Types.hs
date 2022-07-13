{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

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
-- import Data.String.Interpolate (i)
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
  mlmName :: Name,
  mlmClasses :: [Class],
  mlmAssociations :: [Association],
  mlmLinks :: [Link]
} deriving Show

instance Validatable () MLM where
  valid () mlm@(MLM {mlmName, mlmClasses, mlmAssociations, mlmLinks}) = let
    allClassesNames = map cName mlmClasses
    allAssociationsNames = map aName mlmAssociations
    in and [
        not (null mlmClasses),
        valid () mlmName,
        allUnique allClassesNames,
        allUnique allAssociationsNames,
        all (valid mlm) mlmClasses,
        all (valid mlm) mlmAssociations,
        all (valid mlm) mlmLinks
      ]

-- instance Show MLM where
--   show mlm@(MLM {mlmName, mlmClasses, mlmAssociations, mlmLinks}) =
--     [i|#{mlmName} is a#{if valid () mlm then " VALID" else "n INVALID"} Multi-Level Model containing the following:
-- - Classes:
-- #{map show mlmClasses}
-- - Associations:
-- #{map show mlmAssociations}
-- - Links:
-- #{map show mlmLinks}|]

data Class = Class {
  cIsAbstract :: Bool,
  cLevel :: Level,
  cName :: Name,
  cParents :: [Name],
  cOf :: Maybe Name,
  cAttributes :: [Attribute],
  cOperations :: [Operation],
  cSlots :: [Slot]
} deriving (Eq, Show)

getClass :: MLM -> Name -> Maybe Class
getClass (MLM {mlmClasses}) name =
  find ((== name) . cName) mlmClasses

inheritsFrom :: MLM -> Name -> Name -> Bool
inheritsFrom mlm w z =
  maybe False
    (\x -> z `elem` cParents x || any (\y -> inheritsFrom mlm y z) (cParents x))
  (getClass mlm w)

instantiates :: MLM -> Name -> Name -> Bool
instantiates mlm x z =
  maybe False
    (maybe False (\y -> y == z || concretizes mlm y z) . cOf)
  (getClass mlm x)

concretizes :: MLM -> Name -> Name -> Bool
concretizes mlm w z = let
  x = getClass mlm w
  in
    inheritsFrom mlm w z ||
    instantiates mlm w z ||
    maybe False (maybe False (\y -> concretizes mlm y z) . cOf) x ||
    maybe False (any (\y -> concretizes mlm y z) . cParents) x

instance Validatable MLM Class where
  valid mlm (Class {cName = cName', cLevel = cLevel' , cParents, cOf, cAttributes, cOperations, cSlots}) = and [
        valid () cName',
        all (maybe False ((== cLevel') . cLevel) . getClass mlm) cParents,
        allUnique cParents,
        not (concretizes mlm cName' cName'),
        cLevel' > 0 || (null cAttributes && null cOperations),
        all (valid mlm) cAttributes,
        all (valid mlm) cOperations,
        all (valid mlm) cSlots,
        maybe True (maybe True ((== cLevel' + 1) . cLevel) . getClass mlm) cOf
      ]

-- instance Show Class where
--   show (Class {cName, cIsAbstract, cLevel , cParents, cOf, cAttributes}) =
--     [i|  - #{cName}:
--     - #{if cIsAbstract then "abstract" else "not abstract"}
--     - level = #{cLevel}#{maybe "" (\x -> "    - instance of class " ++ show x ++ "\n") cOf}#{if null cParents then "" else "    - inherits from classes " ++ concatMap show cParents ++ "\n"}#{if null cAttributes then "" else "    - attributes: " ++ "\n" ++ concatMap show cAttributes}|]

    -- cOperations = #{map show cOperations}, cSlots = #{map show cSlots}}|]

data Attribute = Attribute {
  tLevel :: Level,
  tName :: Name,
  tType :: Type,
  tClass :: Name,
  tMultiplicity :: Multiplicity
} deriving (Eq, Show)

instance Validatable MLM Attribute where
  valid mlm (Attribute {tMultiplicity, tLevel, tType, tClass, tName}) = and [
      valid () tLevel,
      valid () tName,
      valid () tMultiplicity,
      isUnassigned tType,
      maybe False ((> tLevel) . cLevel) (getClass mlm tClass)
    ]

-- instance Show Attribute where
--   show (Attribute {tLevel, tName, tType, tMultiplicity, tClass}) =
--     [i|Attribute {tClass = #{tClass}, tName = #{tName}, tLevel = #{tLevel}, tType = #{getTypeName tType}, tMultiplicity = #{show tMultiplicity}}|]

data Slot = Slot {
  sAttribute :: Name,
  sClass :: Name,
  sValue :: Type
} deriving (Eq, Show)

instance Validatable MLM Slot where
  valid mlm@(MLM {mlmClasses}) (Slot {sAttribute, sClass, sValue}) = let
    findAttributeClass :: Maybe Class
    findAttributeClass  = find (elem sAttribute . map tName . cAttributes) mlmClasses
--    sAttribute' = maybe Nothing (find ((== sAttribute) . tName) . cAttributes) findAttributeClass
    sAttribute' = ((find ((== sAttribute) . tName) . cAttributes) =<< findAttributeClass)
    in
      maybe False (concretizes mlm sClass . cName) findAttributeClass &&
      maybe False ((\x -> maybe False ((== x) . tLevel) sAttribute') . cLevel) (getClass mlm sClass) &&
      not (isUnassigned sValue)


-- instance Show Slot where
--   show (Slot {sAttribute, sValue}) =
--     [i|Slot {sAttribute = #{sAttribute}, sValue = #{show sValue}}|]

data Operation = Operation {
  oLevel :: Int,
  oName :: Name,
  oType :: Type,
  oIsMonitored :: Bool,
  oBody :: OperationBody,
  oClass :: Name
} deriving (Eq, Show)

instance Validatable MLM Operation where
  valid mlm (Operation {oLevel, oClass, oType}) =
    maybe False ((> oLevel) . cLevel) (getClass mlm oClass) &&
    isUnassigned oType

-- instance Show Operation where
--   show (Operation {oLevel, oName, oType, oIsMonitored, oBody}) =
--     [i|Operation {oName = #{oName}, oLevel = #{oLevel}, oType = #{show oType}, oIsMonitored = #{oIsMonitored}, oBody = #{oBody}}|]

data OperationBody = OperationBody {
  placeholder1 :: String,
  placeholder2 :: String
} deriving (Eq, Show)

instance Validatable mlm OperationBody where
  valid _ _ = True --placeholder

-- instance Show OperationBody where
--   show (OperationBody {placeholder1, placeholder2}) =
--     "placeholder : operation body" ++
--     placeholder1 ++
--     placeholder2

data Association = Association {
  aName :: Name,
  aSource :: Name,
  aTarget :: Name,
  aLvlSource :: Level,
  aLvlTarget :: Level,
  aMultTargetToSource :: Multiplicity,
  aMultSourceToTarget :: Multiplicity,
  aSourceVisibleFromTarget :: Bool,
  aTargetVisibleFromSource :: Bool
} deriving (Eq, Show)

getAssociation :: MLM -> Name -> Maybe Association
getAssociation (MLM {mlmAssociations}) name =
  find ((== name) . aName) mlmAssociations

instance Validatable MLM Association where
  valid mlm (Association {aSource, aTarget, aLvlSource, aLvlTarget, aMultTargetToSource, aMultSourceToTarget, aName}) = and [
    valid () aName,
    maybe False ((> aLvlSource) . cLevel) (getClass mlm aSource),
    maybe False ((> aLvlTarget) . cLevel) (getClass mlm aTarget),
    valid () aMultTargetToSource,
    valid () aMultSourceToTarget
    ]

-- instance Show Association where
--   show (Association {aName, aSource, aTarget, aLvlSource, aLvlTarget, aMultTargetToSource, aMultSourceToTarget, aSourceVisibleFromTarget, aTargetVisibleFromSource}) =
--     [i|Association {aName = #{aName}, aSource = #{aSource}, aTarget = #{aTarget}, aLvlSource = #{aLvlSource}, aLvlTarget = #{aLvlTarget}, aMultTargetToSource = #{show aMultTargetToSource}, aMultSourceToTarget = #{show aMultSourceToTarget}, aSourceVisibleFromTarget = #{aSourceVisibleFromTarget}, aTargetVisibleFromSource = #{aTargetVisibleFromSource}}|]

data Link = Link {
  lAssociation :: Name,
  lSource :: Name,
  lTarget :: Name
} deriving (Eq, Show)

instance Validatable MLM Link where
  valid mlm (Link {lAssociation, lSource, lTarget}) = let
    lAssociation' = getAssociation mlm lAssociation
    check condition = maybe False condition lAssociation'
    in
    and [
      check (concretizes mlm lSource . aSource),
      check (concretizes mlm lTarget . aTarget),
      check (\x -> maybe False ((== aLvlSource x) . cLevel) (getClass mlm lSource)),
      check (\x -> maybe False ((== aLvlTarget x) . cLevel) (getClass mlm lTarget))
    ]

-- instance Show Link where
--   show (Link {lAssociation, lSource, lTarget}) =
--     [i|Link {lAssociation = #{lAssociation}, lSource = #{lSource}, lTarget = #{lTarget}}|]

data Multiplicity = Multiplicity {
  lower :: Int,
  upper :: Int
} deriving (Eq, Show)

instance Validatable () Multiplicity where
  valid () (Multiplicity {lower, upper}) =
    lower >= 0 &&
    upper <= upper || upper == -1

-- instance Show Multiplicity where
--   show (Multiplicity {lower, upper}) =
--     show (lower, upper)

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
