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
  Value (..),
  XModelerCurrency (..),
  valid,
  relativeToEur,
  currencySymbol,
  emptyName,
  emptyMLM,
  emptyClass,
  emptyAssociation,
  emptyLink,
  emptyAttribute,
  emptySlot,
  emptyOperation,
  attributeTypeSpace,
) where

import Data.List.UniqueStrict (allUnique)
import Data.Char (isDigit)
import Data.List (find, sort, group)
import Data.Ix (inRange)
import Data.Maybe (isJust, isNothing, maybeToList)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))

noCycles :: Eq a => [(a, a)] -> Bool
noCycles [] = True
noCycles list = let peeled = filter (isJust . flip lookup list . snd) list in
  (length peeled < length list) && noCycles peeled

eqBy :: Eq b => a -> a -> (a -> b) -> Bool
eqBy x y f = f x == f y

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

class Validatable context a where
  valid :: context -> a -> Bool

newtype Name = Name String deriving (Eq, Ord)

emptyName :: Name
emptyName = Name ""

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

instance Eq MLM where
  (==) MLM{classes = c1, associations = a1, links = l1} MLM{classes = c2, associations = a2, links = l2} = let
    f :: (Eq a, Ord a) => ([a],[a]) -> Bool
    f = uncurry (==) . both nubSort
    in f (c1,c2) && f (a1,a2) && f (l1,l2)

emptyMLM :: MLM
emptyMLM = MLM emptyName [] [] []

instance Validatable () MLM where
  valid () (MLM {name = projectName, classes, associations, links}) = let
    allClassesNames = map #name classes :: [Name]

    -- navigation
    getClass className = find ((== className) . #name) classes
    getAssociation link' = find ((== #association link') . #name) associations

    parentDictList :: [(Name, [Name])]
    parentDictList = map (\(Class {name, parents}) -> (name, parents)) classes

    classifierDictList :: [(Name, [Name])]
    classifierDictList = filter (not . null . snd) $
      map (\(Class {name, classifier}) -> (name, maybeToList classifier))
      classes

    unlistDict :: [(a, [b])] -> [(a, b)]
    unlistDict = concatMap (\(x, xs) -> map (x, ) xs)

    parentDict :: [(Name, Name)]
    parentDict = unlistDict parentDictList

    classifierDict :: [(Name, Name)]
    classifierDict = unlistDict classifierDictList

    dict :: [(Name, Name)]
    dict = parentDict ++ classifierDict

    getClassifier :: Name -> Maybe Name
    getClassifier x = lookup x classifierDict

    -- whether x concretizes or inherits from z
    (\/) :: Name -> Name -> Bool
    (\/) x z = maybe False (\y -> y==z || y \/ z) $ lookup x dict

    -- whether an association multiplicity is violated:
    linksOf :: Association -> [Link]
    linksOf x = filter ((#name x ==) . #association) links
    sourcesOf :: Association -> [Name]
    sourcesOf = map #source . linksOf
    targetsOf :: Association -> [Name]
    targetsOf = map #target . linksOf
    inRangeOfMult :: Multiplicity -> Int -> Bool
    inRangeOfMult (Multiplicity (a, Nothing)) x = x >= a
    inRangeOfMult (Multiplicity (a, Just b)) x = inRange (a, b) x
    allInRangeOfMult :: Multiplicity -> [Name] -> Bool
    allInRangeOfMult mult = all (inRangeOfMult mult . length) . group . sort

    multNotViolated :: Association -> Bool
    multNotViolated x@(Association {multSourceToTarget, multTargetToSource}) =
      allInRangeOfMult multSourceToTarget (sourcesOf x) &&
      allInRangeOfMult multTargetToSource (targetsOf x)

    -- whether a class concretizes a class whose level is not higher by 1
    lvlIsClassifierLvlMinusOne class' =
      maybe True
        (maybe False
          (\classifierExisting -> #level classifierExisting == #level class' + 1)
          . getClass
      ) (#classifier class')

    isLinked :: Name -> Bool
    isLinked className = any (\(Link {source, target}) -> source == className || target == className) links

    instantiatesSomethingOrIsMetaClass :: Class -> Bool
    instantiatesSomethingOrIsMetaClass (Class {slots, classifier, operations, name}) = or [
      isNothing classifier,
      isLinked name ,
      not (null operations),
      not (null slots)
      ]

    -- whether source of link concretizes or inherits from source of association of that link and
    -- whether target of link concretizes or inherits from target of association of that link
    checkSourceAndTarget x = maybe False (\asso ->
        #source x \/ #source asso && #target x \/ #target asso
      ) (getAssociation x)

    -- determine scope:
    scope :: Class -> [Class]
    scope x = filter ((#name x \/) . #name) classes

    in and [
      not (null classes),
      valid () projectName,
      allUnique allClassesNames,
      allUnique (map #name associations),
      noCycles dict,
      all lvlIsClassifierLvlMinusOne classes,
      all instantiatesSomethingOrIsMetaClass classes,
      all (\(x, y) -> getClassifier x == getClassifier y) parentDict,
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
} deriving Show

instance Ord Class where
  compare x y = compare (#name x) (#name y)

instance Eq Class where
  (==) x y = and [
      eqBy x y #isAbstract,
      eqBy x y #level,
      eqBy x y #name,
      eqBy (#parents x) (#parents y) nubSort,
      eqBy x y #classifier,
      eqBy (#attributes x) (#attributes y) nubSort,
      eqBy (#operations x) (#operations y) nubSort,
      eqBy (#slots x) (#slots y) nubSort
    ]

emptyClass :: Class
emptyClass = Class False 0 emptyName [] Nothing [] [] []

instance Validatable ([Class], [Maybe Class]) Class where
  valid (classScope, parentsClasses) (Class {name = className, level = level', attributes = attributes', operations, slots, parents}) = let
    getAttributeClass x = find (elem x . map #name . #attributes) classScope
    getAttribute x = ((find ((== x) . #name) . #attributes) =<< getAttributeClass x)
    in
    and [
        valid () className,
        valid () level',
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
  dataType :: Type,
  multiplicity :: Multiplicity
} deriving Show

instance Ord Attribute where
  compare x y = compare (#name x) (#name y)

instance Eq Attribute where
  (==) x y = and [eqBy x y #level, eqBy x y #name, eqBy x y #dataType, eqBy x y #multiplicity]

emptyAttribute :: Attribute
emptyAttribute = Attribute 0 emptyName Boolean emptyMultiplicity

instance Validatable Level Attribute where
  valid classLevel (Attribute {multiplicity, level = attributeLevel, dataType, name}) = and [
      valid () attributeLevel,
      valid () name,
      valid () multiplicity,
      isUnassigned dataType,
      classLevel > attributeLevel
    ]

data Slot = Slot {
  name :: Name,
  value :: Value
} deriving Show

instance Ord Slot where
  compare x y = let
    f = #name :: Slot -> Name
    in compare (f x) (f y)

instance Eq Slot where
  (==) x y = let
    f = #name :: Slot -> Name
    g = #value :: Slot -> Value
    in eqBy x y f && eqBy x y g

emptySlot :: Slot
emptySlot = Slot emptyName emptyValue

instance Validatable (Maybe Attribute, Level) Slot where
  valid (slotAttribute, slotClassLvl) (Slot {value}) =
      maybe False ((slotClassLvl ==) . #level) slotAttribute
      &&
      not (isUnassigned value)

data Operation = Operation {
  level :: Int,
  name :: Name,
  dataType :: Type,
  isMonitored :: Bool,
  body :: OperationBody
} deriving Show

instance Ord Operation where
  compare x y = compare (#name x) (#name y)

instance Eq Operation where
  (==) x y = and [eqBy x y #level, eqBy x y #name, eqBy x y #dataType, eqBy x y #isMonitored, eqBy x y #body]

emptyOperation :: Operation
emptyOperation = Operation 0 emptyName Boolean False emptyOperationBody

instance Validatable Level Operation where
  valid operationClassLvl (Operation {level , dataType}) =
    operationClassLvl > level && isUnassigned dataType

data OperationBody = OperationBody {
  placeholder1 :: String,
  placeholder2 :: String
} deriving Show

instance Eq OperationBody where
  (==) x y = all (eqBy x y) [#placeholder1, #placeholder2]

emptyOperationBody :: OperationBody
emptyOperationBody = OperationBody "" ""

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
} deriving Show

instance Ord Association where
  compare x y = compare (#name x) (#name y)

instance Eq Association where
  (==) x y = and [
    eqBy x y #name,
    eqBy x y #source,
    eqBy x y #target,
    eqBy x y #lvlSource,
    eqBy x y #lvlTarget,
    eqBy x y #multTargetToSource,
    eqBy x y #multSourceToTarget,
    eqBy x y #sourceVisibleFromTarget,
    eqBy x y #targetVisibleFromSource]

emptyAssociation :: Association
emptyAssociation = Association emptyName emptyName emptyName 0 0 emptyMultiplicity emptyMultiplicity False False

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
  name :: Name,
  source :: Name,
  target :: Name
} deriving Show

instance Ord Link where
  compare x y = let
    f = #name :: Link -> Name
    in compare (f x) (f y)

instance Eq Link where
  (==) x y = all (eqBy x y) [#name, #source, #target]

emptyLink :: Link
emptyLink = Link emptyName emptyName emptyName

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

newtype Multiplicity = Multiplicity (Int, Maybe Int) deriving (Eq, Show)

emptyMultiplicity :: Multiplicity
emptyMultiplicity = Multiplicity (0, Nothing)

instance Validatable () Multiplicity where
  valid () (Multiplicity (lower, Nothing)) = lower >= 0
  valid () (Multiplicity (lower, Just upper)) = lower >= 0 && lower <= upper

type Level = Int

instance Validatable () Level where
  valid () level = level >= 0

data Type = Boolean | Integer | Float | String | Element | MonetaryValue | Date | Currency | Complex | AuxiliaryClass deriving (Eq, Show, Read)

data Value =
  VBoolean Bool |
  VInteger Int |
  VFloat Float |
  VString String |
  VElement String |
  VMonetaryValue (String, String) |
  VDate (Int, Int, Int) |
  VCurrency XModelerCurrency |
  VComplex String |
  VAuxiliaryClass String
  deriving (Eq, Show, Read)

emptyValue :: Value
emptyValue = VBoolean False

attributeTypeSpace :: [Type]
attributeTypeSpace = [Boolean,Integer,Float,String,Element,MonetaryValue,Date,Currency,Complex,AuxiliaryClass]

equalType :: Type -> Value -> Bool
equalType Boolean (VBoolean _) = True
equalType Integer (VInteger _) = True
equalType Float (VFloat _) = True
equalType String (VString _) = True
equalType Element (VElement _) = True
equalType MonetaryValue (VMonetaryValue _) = True
equalType Date (VDate _) = True
equalType Currency (VCurrency _) = True
equalType Complex (VComplex _) = True
equalType AuxiliaryClass (VAuxiliaryClass _) = True
equalType _ _ = False

data XModelerCurrency = USD | EUR | GBP | AUD | NZD deriving (Eq, Show, Read)

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
