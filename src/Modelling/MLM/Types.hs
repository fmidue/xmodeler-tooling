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
  Level,
  Type (..),
  Value (..),
  relativeToEur,
  currencySymbol,
  allCurrencies,
  emptyName,
  emptyClass,
  emptyAssociation,
  emptyLink,
  emptyAttribute,
  emptyOperation,
  emptyOperationBody,
  typeSpace,
  (\?/),
  generateAboveFinder,
  generateBelowFinder,
  generateClassDict,
  generateClassFinder,
  generateInstantiatableAttributesFinder,
  generateInstantiatableOperationsFinder,
  generateOccurrencesCounter,
  equalType,
  inRangeOfMult
) where

import Data.List (sort, sortOn)
import Data.Ix (inRange)
import Data.Maybe (maybeToList)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import Data.Map.Strict (Map, (!), (!?), fromList)
import qualified Data.Map.Lazy as Lazy (Map, fromList)
import Data.Map.Lazy (keysSet)
import qualified Data.Map.Lazy as M (filter)
import Data.Set (member)
import Data.List.Extra (replace)


eqBy :: Eq b => a -> a -> (a -> b) -> Bool
eqBy x y f = f x == f y

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

newtype Name = Name String deriving (Eq, Ord, Show, Read)

emptyName :: Name
emptyName = Name ""

data MLM = MLM {
  name :: Name,
  classes :: [Class],
  associations :: [Association],
  links :: [Link]
} deriving (Show, Read)

instance Eq MLM where
  (==) MLM{classes = c1, associations = a1, links = l1} MLM{classes = c2, associations = a2, links = l2} =
    eqBy c1 c2 (sortOn #name) &&
    eqBy a1 a2 (sortOn #name) &&
    eqBy l1 l2 sort

generateClassDict :: [Class] -> Map Name [Name]
generateClassDict =
  fromList . map (\Class{name, classifier, parents} -> (name, maybeToList classifier ++ parents))

(\?/) :: Either [Class] ([Name], Map Name [Name]) -> (Name -> Name -> Bool)
(\?/) (Left theClasses) = (\?/) (Right (map #name theClasses, generateClassDict theClasses))
(\?/) (Right (theNames, classDict)) = let
  f :: Name -> Name -> Bool
  x `f` y = let list = classDict ! x
    in y `elem` list || any ((f_tabled !) . (, y)) list
  f_tabled :: Lazy.Map (Name, Name) Bool
  f_tabled = Lazy.fromList [ ( (x, y), x `f` y ) | x <- theNames, y <- theNames ]
  in curry (`member` keysSet (M.filter id f_tabled))

generateAboveFinder :: [Class] -> Maybe (Name -> Name -> Bool) -> (Name -> [Class])
generateAboveFinder theClasses Nothing = let
  (\/) :: Name -> Name -> Bool
  (\/) = (\?/) (Left theClasses)
  in generateAboveFinder theClasses (Just (\/))
generateAboveFinder theClasses (Just (\/)) =
  (!) $ fromList (map (\Class{name} -> (name, filter ((name \/) . #name) theClasses)) theClasses)

generateBelowFinder :: [Class] -> (Name -> [Class])
generateBelowFinder theClasses = let
  (\/) :: Name -> Name -> Bool
  (\/) = (\?/) (Left theClasses)
  in
  (!) $ fromList (map (\Class{name} -> (name, filter ((\/ name) . #name) theClasses)) theClasses)

generateClassFinder :: [Class] -> (Name -> Maybe Class)
generateClassFinder = (!?) . fromList . map (\c@Class{name} -> (name, c))

generateInstantiatableAttributesFinder :: [Class] -> (Class -> [Attribute])
generateInstantiatableAttributesFinder theClasses = let
  above :: Name -> [Class]
  above = generateAboveFinder theClasses Nothing
  in
  \Class{name, level = classLevel} ->
    filter ((== classLevel) . #level) $ concatMap #attributes $ above name

generateInstantiatableOperationsFinder :: (Name -> [Class]) -> (Class -> [Operation])
generateInstantiatableOperationsFinder above Class{name, level = classLevel} =
  filter ((== classLevel) . #level) $ concatMap #operations $ above name

generateOccurrencesCounter :: Bool -> [Link] -> Class -> Association -> Int
generateOccurrencesCounter asSourceRatherThanAsTarget theLinks Class{name = className} Association{name = associationName} = let
  toLookFor = if asSourceRatherThanAsTarget then #source else #target in
    length $ filter (\link ->
        #name link == associationName && toLookFor link == className
      ) theLinks

inRangeOfMult :: Int -> Multiplicity -> Bool
inRangeOfMult x (Multiplicity (a, Nothing)) = x >= a
inRangeOfMult x (Multiplicity (a, Just b)) = inRange (a, b) x

data Class = Class {
  isAbstract :: Bool,
  level :: Level,
  name :: Name,
  parents :: [Name],
  classifier :: Maybe Name,
  attributes :: [Attribute],
  operations :: [Operation],
  slots :: [Slot]
} deriving (Show, Read)

instance Eq Class where
  (==) x y = and [
      eqBy x y #isAbstract,
      eqBy x y #level,
      eqBy x y #name,
      eqBy x y (sort . #parents),
      eqBy x y #classifier,
      eqBy x y (sortOn #name . #attributes),
      eqBy x y (sortOn #name . #operations),
      eqBy x y (sortOn #name . #slots)
    ]

emptyClass :: Class
emptyClass = Class False 0 emptyName [] Nothing [] [] []

data Attribute = Attribute {
  level :: Level,
  name :: Name,
  dataType :: Type,
  multiplicity :: Multiplicity
} deriving (Show, Read, Eq)

emptyAttribute :: Attribute
emptyAttribute = Attribute 0 emptyName Boolean emptyMultiplicity

data Slot = Slot {
  name :: Name,
  value :: Value
} deriving (Show, Read, Eq)

data Operation = Operation {
  level :: Int,
  name :: Name,
  dataType :: Type,
  isMonitored :: Bool,
  body :: String
} deriving (Show, Read)

emptyOperationBody :: Name -> String
emptyOperationBody (Name name') = "@Operation " ++ name' ++ "[monitor=false]():XCore::Integer&#99;  0&#10;end"

instance Eq Operation where
  (==) x y = and [
      eqBy x y #level,
      eqBy x y #name,
      eqBy x y #dataType,
      eqBy x y #isMonitored,
      eqBy x y (replace "&#10;" "\n" . #body)
    ]

emptyOperation :: Operation
emptyOperation = Operation 0 emptyName Boolean False ""

data Association = Association {
  name :: Name,
  source :: Name,
  target :: Name,
  lvlSource :: Level,
  lvlTarget :: Level,
  multSource :: Multiplicity,
  multTarget :: Multiplicity,
  visibleSource :: Bool,
  visibleTarget :: Bool
} deriving (Show, Read, Eq)

emptyAssociation :: Association
emptyAssociation = Association emptyName emptyName emptyName 0 0 emptyMultiplicity emptyMultiplicity False False

data Link = Link {
  name :: Name,
  source :: Name,
  target :: Name
} deriving (Show, Read, Eq, Ord)

emptyLink :: Link
emptyLink = Link emptyName emptyName emptyName

newtype Multiplicity = Multiplicity (Int, Maybe Int) deriving (Eq, Show, Read)

emptyMultiplicity :: Multiplicity
emptyMultiplicity = Multiplicity (0, Nothing)

type Level = Int

data Type = Boolean | Integer | Float | String | Element | MonetaryValue | Date | Currency | Complex | AuxiliaryClass deriving (Eq, Show, Read, Enum, Bounded)

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

typeSpace :: [Type]
typeSpace = [minBound .. maxBound]

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

data XModelerCurrency = USD | EUR | GBP | AUD | NZD deriving (Eq, Show, Read, Enum, Bounded)

allCurrencies :: [XModelerCurrency]
allCurrencies = [minBound .. maxBound]

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
