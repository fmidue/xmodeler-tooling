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
  Name,
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
import Data.String.Interpolate (i)
import Data.List.Split (splitOn)

class Validatable a where
  valid :: a -> Bool

type Name = String

instance Validatable Name where
  valid name = let
    validChar1 = flip elem (['a'..'z'] ++ ['A'..'Z'])
    validCharN char = validChar1 char || isDigit char || char == '_'
    in
      all ($ name)
        [not . null, validChar1 . head, all validCharN . tail]

data MLM = MLM {
  mlmName :: Name,
  mlmClasses :: [Class],
  mlmAssociations :: [Association],
  mlmLinks :: [Link]
}

instance Validatable MLM where
  valid (MLM {mlmName, mlmClasses, mlmAssociations, mlmLinks}) = let
    allClassesNames = map cName mlmClasses
    allAssociationsNames = map aName mlmAssociations
    in and [
        valid mlmName,
        allUnique allClassesNames,
        allUnique allAssociationsNames,
        all valid mlmClasses,
        all valid mlmAssociations,
        all valid mlmLinks
      ]

instance Show MLM where
  show (MLM {mlmName, mlmClasses, mlmAssociations, mlmLinks}) =
    [i|MLM {mlmName = #{mlmName}, mlmClasses = #{map show mlmClasses}, mlmAssociations = #{map show mlmAssociations}, mlmLinks = #{map show mlmLinks}}|]

data Class = Class {
  cIsAbstract :: Bool,
  cLevel :: Level,
  cName :: Name,
  cParents :: [Class],
  cOf :: Maybe Class,
  cAttributes :: [Attribute],
  cOperations :: [Operation],
  cSlots :: [Slot]
} deriving (Eq)

concretizes :: Class -> Class -> Bool
concretizes (Class {cOf}) y =
  case cOf of
    Just x -> x == y || x `concretizes` y
    _ -> False

inheritsFrom :: Class -> Class -> Bool
inheritsFrom (Class {cParents}) y =
  case cParents of
    [] -> False
    cParents' -> y `elem` cParents' || any (`inheritsFrom` y) cParents'

instance Validatable Class where
  valid class'@(Class {cName = cName', cLevel = cLevel' , cParents, cOf, cAttributes, cOperations, cSlots}) = and [
    valid cName',
    all ((== cLevel') . cLevel) cParents,
    allUnique (map cName cParents),
    not (class' `inheritsFrom` class'),
    cLevel' > 0 || (null cAttributes && null cOperations),
    all valid cAttributes,
    all valid cOperations,
    all valid cSlots,
    case cOf of
      Just cOf' -> cLevel cOf' == cLevel' + 1
      Nothing -> True
    ]

instance Show Class where
  show (Class {cName = cName', cIsAbstract, cLevel , cParents, cOf, cAttributes, cOperations, cSlots}) = let
    cOf' = maybe "" cName cOf
    in
    [i|Class {cIsAbstract = #{cIsAbstract}, cName = #{cName'}, cLevel = #{cLevel}, cOf = #{cOf'}, cParents = #{map cName cParents}, cAttributes = #{map show cAttributes}, cOperations = #{map show cOperations}, cSlots = #{map show cSlots}}|]

data Attribute = Attribute {
  tLevel :: Level,
  tName :: Name,
  tType :: Type,
  tClass :: Class,
  tMultiplicity :: Multiplicity
} deriving (Eq)

instance Validatable Attribute where
  valid (Attribute {tMultiplicity, tLevel, tType, tClass, tName}) = and [
      valid tLevel,
      valid tName,
      valid tMultiplicity,
      isUnassigned tType,
      cLevel tClass > tLevel
    ]

instance Show Attribute where
  show (Attribute {tLevel, tName, tType, tMultiplicity, tClass}) =
    [i|Attribute {tClass = #{cName tClass}, tName = #{tName}, tLevel = #{tLevel}, tType = #{getTypeName tType}, tMultiplicity = #{show tMultiplicity}}|]

data Slot = Slot {
  sAttribute :: Attribute,
  sClass :: Class,
  sValue :: Type
} deriving (Eq)

instance Validatable Slot where
  valid (Slot {sAttribute, sClass, sValue}) =
    sClass `concretizes` tClass sAttribute &&
    tLevel sAttribute == cLevel sClass &&
    not (isUnassigned sValue)

instance Show Slot where
  show (Slot {sAttribute, sValue}) =
    [i|Slot {sAttribute = #{tName sAttribute}, sValue = #{show sValue}}|]

data Operation = Operation {
  oLevel :: Int,
  oName :: Name,
  oType :: Type,
  oIsMonitored :: Bool,
  oBody :: OperationBody,
  oClass :: Class
} deriving (Eq)

instance Validatable Operation where
  valid (Operation {oLevel, oClass, oType}) =
    oLevel < cLevel oClass &&
    isUnassigned oType

instance Show Operation where
  show (Operation {oLevel, oName, oType, oIsMonitored, oBody}) =
    [i|Operation {oName = #{oName}, oLevel = #{oLevel}, oType = #{show oType}, oIsMonitored = #{oIsMonitored}, oBody = #{oBody}}|]

data OperationBody = OperationBody {
  placeholder1 :: String,
  placeholder2 :: String
} deriving (Eq)

instance Validatable OperationBody where
  valid _ = True --placeholder

instance Show OperationBody where
  show (OperationBody {placeholder1, placeholder2}) =
    "placeholder : operation body" ++
    placeholder1 ++
    placeholder2

data Association = Association {
  aName :: Name,
  aSource :: Class,
  aTarget :: Class,
  aLvlSource :: Level,
  aLvlTarget :: Level,
  aMultTargetToSource :: Multiplicity,
  aMultSourceToTarget :: Multiplicity,
  aSourceVisibleFromTarget :: Bool,
  aTargetVisibleFromSource :: Bool
} deriving (Eq)

instance Validatable Association where
  valid (Association {aSource, aTarget,
    aLvlSource, aLvlTarget, aMultTargetToSource, aMultSourceToTarget, aName}) = and [
    valid aName,
    aLvlSource < cLevel aSource,
    aLvlTarget < cLevel aTarget,
    valid aMultTargetToSource,
    valid aMultSourceToTarget
    ]

instance Show Association where
  show (Association {aName, aSource, aTarget, aLvlSource, aLvlTarget, aMultTargetToSource, aMultSourceToTarget, aSourceVisibleFromTarget, aTargetVisibleFromSource}) =
    [i|Association {aName = #{aName}, aSource = #{cName aSource}, aTarget = #{cName aTarget}, aLvlSource = #{aLvlSource}, aLvlTarget = #{aLvlTarget}, aMultTargetToSource = #{show aMultTargetToSource}, aMultSourceToTarget = #{show aMultSourceToTarget}, aSourceVisibleFromTarget = #{aSourceVisibleFromTarget}, aTargetVisibleFromSource = #{aTargetVisibleFromSource}}|]

data Link = Link {
  lAssociation :: Association,
  lSource :: Class,
  lTarget :: Class
} deriving (Eq)

instance Validatable Link where
  valid (Link {lAssociation, lSource, lTarget}) = and [
    lSource `concretizes` aSource lAssociation,
    lTarget `concretizes` aTarget lAssociation,
    cLevel lSource == aLvlSource lAssociation,
    cLevel lTarget == aLvlTarget lAssociation
    ]

instance Show Link where
  show (Link {lAssociation, lSource, lTarget}) =
    [i|Link {lAssociation = #{aName lAssociation}, lSource = #{cName lSource}, lTarget = #{cName lTarget}}|]

data Multiplicity = Multiplicity {
  lower :: Int,
  upper :: Int
} deriving (Eq)

instance Validatable Multiplicity where
  valid (Multiplicity {lower, upper}) =
    lower >= 0 &&
    upper <= upper || upper == -1


instance Show Multiplicity where
  show (Multiplicity {lower, upper}) =
    show (lower, upper)

type Level = Int

instance Validatable Level where
  valid level = level >= 0

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
