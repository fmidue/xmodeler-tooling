{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}
module Modelling.CdOd.Types (
  Association,
  AssociationType (..),
  Change (..),
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  Letters (..),
  Name (..),
  NameMapping (..),
  ObjectConfig (..),
  Od,
  RelationshipProperties (..),
  Syntax,
  addedAssociation,
  associationNames,
  checkClassConfig,
  checkClassConfigWithProperties,
  classNames,
  classNamesOd,
  defaultProperties,
  fromNameMapping,
  linkNames,
  maxFiveObjects,
  maxRels,
  parseLettersPrec,
  parseNamePrec,
  renameAssocsInCd,
  renameAssocsInEdge,
  renameClassesInCd,
  renameClassesInEdge,
  renameClassesInOd,
  renameLinksInOd,
  showLetters,
  showName,
  toNameMapping,
  toOldSyntax,
  ) where

import qualified Data.Bimap                       as BM

import Modelling.Auxiliary.Common       (skipSpaces)

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad                    (void)
import Control.Monad.Catch              (MonadThrow)
import Data.Bifunctor                   (first, second)
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (bimapM)
import Data.Char                        (isAlpha, isAlphaNum)
import Data.List                        (intercalate, nub)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (fromMaybe, listToMaybe)
import Data.String                      (IsString (fromString))
import Data.String.Interpolate          (iii)
import GHC.Generics                     (Generic)
import Text.ParserCombinators.Parsec (
  Parser,
  many1,
  satisfy,
  endBy,
  )

type Od = ([String], [(Int, Int, String)])

type Association = (AssociationType, String, (Int, Maybe Int), String, String, (Int, Maybe Int))

data AssociationType = Association | Aggregation | Composition
  deriving (Eq, Generic, Read, Show)

data Connection = Inheritance | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving (Eq, Generic, Read, Show)

type Syntax = ([(String, [String])], [Association])

type DiagramEdge = (String, String, Connection)

newtype Name = Name { unName :: String }
  deriving (Eq, Generic, Ord, Read, Show)

instance IsString Name where
  fromString = Name

showName :: Name -> String
showName = unName

parseNamePrec :: Int -> Parser Name
parseNamePrec _ = do
  skipSpaces
  Name <$> many1 (satisfy isAlphaNum)

newtype Letters = Letters { lettersList :: String }
  deriving (Eq, Generic, Ord, Read, Show)

instance IsString Letters where
  fromString = Letters

showLetters :: Letters -> String
showLetters = lettersList

parseLettersPrec :: Int -> Parser Letters
parseLettersPrec _ = do
  skipSpaces
  Letters <$> endBy (satisfy isAlpha) skipSpaces

newtype NameMapping = NameMapping { nameMapping :: Bimap Name Name }
  deriving (Eq, Generic)

fromNameMapping :: NameMapping -> Bimap String String
fromNameMapping = BM.mapMonotonic unName . BM.mapMonotonicR unName . nameMapping

toNameMapping :: Bimap String String -> NameMapping
toNameMapping = NameMapping . BM.mapMonotonic Name . BM.mapMonotonicR Name

instance Show NameMapping where
  show = show . BM.toList . nameMapping

instance Read NameMapping where
  readsPrec p xs = [(NameMapping $ BM.fromList y, ys) | (y, ys) <- readsPrec p xs]

data Change a = Change {
    add    :: Maybe a,
    remove :: Maybe a
  } deriving (Foldable, Functor, Generic, Read, Show, Traversable)

data ClassConfig = ClassConfig {
    classes      :: (Int, Int),
    aggregations :: (Int, Maybe Int),
    associations :: (Int, Maybe Int),
    compositions :: (Int, Maybe Int),
    inheritances :: (Int, Maybe Int),
    -- | the number of relationships including inheritances
    relationships :: (Int, Maybe Int)
  } deriving (Eq, Generic, Read, Show)

checkClassConfigWithProperties
  :: ClassConfig
  -> RelationshipProperties
  -> Maybe String
checkClassConfigWithProperties
  c@ClassConfig {..}
  RelationshipProperties {..}
  | wrongAssocs > maxRelations - fst inheritances
  || maybe False (wrongAssocs >) maxAssocs'
  = Just [iii|
    The (maximum) number of non-inheritance relationships is too low for
    the targeted wrongAssocs!
    |]
  | wrongCompositions > maxCompositions
  || maybe False (wrongCompositions >) (snd compositions)
  = Just [iii|
    The (maximum) number of possible compositions is too low for
    the targeted wrongCompositions!
    |]
  | minCompositions > maxCompositions
  || maybe False (minCompositions >) (snd compositions)
  = Just [iii|
    The (maximum) number of possible compositions is too low for
    the targeted composition properties!
    |]
  | minCompositionsInheritances > maxCompositionsInheritances
  || maybe False (minCompositionsInheritances >) maxCompositionsInheritances'
  = Just [iii|
    The (maximum) number of possible compositions or inheritances is too low for
    creating composition cycles!
    |]
  | minAssocs > maxRelations - fst inheritances
  || maybe False (minAssocs >) maxAssocs'
  = Just [iii|
    The (maximum) number of possible non-inheritance relationships is too low for
    the targeted non-inheritance relationship properties!
    |]
  | minInheritances > maxInheritances
  || maybe False (minInheritances >) (snd inheritances)
  = Just [iii|
    The (maximum) number of possible inheritance relationships is too low for
    the targeted inheritance relationship properties!
    |]
  | Just x <- snd relationships, Just rels <- relationshipsSum c, x > rels
  = Just [iii|
    The maximum number of relationships is too high
    according to individual relationship maxima!
    |]
  | x@Just {} <- snd relationships
  , any ((> x) . snd) [aggregations, associations, compositions, inheritances]
  = Just [iii|
    The maximum number of aggregations, associations, compositions
    as well as inheritances
    must not be higher than the maximum number of relationships!
    |]
  | otherwise = checkClassConfig c
  where
    for x y = if y then x else 0
    plusOne x = if x /= 0 then x + 1 else x
    minAssocs = (+ selfRelationships) . plusOne $ sum [
      1 `for` hasDoubleRelationships,
      1 `for` hasReverseRelationships
      ]
    minInheritances = (+ selfInheritances) . plusOne $ sum [
      1 `for` hasReverseInheritances,
      1 `for` hasMultipleInheritances,
      2 `for` hasNonTrivialInheritanceCycles
      ]
    minCompositions = max
      (1 `for` hasCompositionCycles)
      (2 `for` hasCompositionsPreventingParts)
    minCompositionsInheritances =
      3 `for` hasCompositionCycles
    maxRelations = fromMaybe (maxRels c) $ snd relationships
    maxCompositionsInheritances = maxRelations
      - fst aggregations
      - fst associations
    maxCompositions = maxCompositionsInheritances - fst inheritances
    maxInheritances = maxCompositionsInheritances - fst compositions
    maxCompositionsInheritances' = (+)
      <$> snd compositions
      <*> snd inheritances
    maxAssocs' = (\x y z -> x + y + z)
      <$> snd aggregations
      <*> snd associations
      <*> snd compositions

checkClassConfig :: ClassConfig -> Maybe String
checkClassConfig c@ClassConfig {..} = checkRange Just "classes" classes
  <|> checkRange id "aggregations" aggregations
  <|> checkRange id "associations" associations
  <|> checkRange id "compositions" compositions
  <|> checkRange id "inheritances" inheritances
  <|> checkRange id "relationships" relationships
  <|> toMaybe (fst relationships < minRels c) [iii|
      The sum of the minimum number of aggregations, associations, compositions
      and inheritances
      must not be higher than the minimum number of relationships!
      |]
  <|> do
    void $ snd relationships
    toMaybe isMaxHigherThanAnyIndividual [iii|
      The maximum number of aggregations, associations, compositions
      as well as inheritances
      must not be higher than the maximum number of relationships!
      |]
  where
    toMaybe True x = Just x
    toMaybe _    _ = Nothing
    isMaxHigherThanAnyIndividual = any
      ((> snd relationships) . snd)
      [aggregations, associations, compositions, inheritances]

checkRange
  :: (Num n, Ord n, Show b, Show n)
  => (b -> Maybe n)
  -> String
  -> (n, b)
  -> Maybe String
checkRange g what (low, h) = do
  high <- g h
  assert high
  where
    assert high
      | low < 0 = Just @String [iii|
        The lower limit for #{what} has to be at least 0!
        |]
      | high < low = Just [iii|
        The upper limit (currently #{show h}; second value) for #{what}
        has to be as high as its lower limit
        (currently #{show low}; first value)!
        |]
      | otherwise = Nothing

minRels :: ClassConfig -> Int
minRels ClassConfig {..} =
  fst aggregations
  + fst associations
  + fst compositions
  + fst inheritances

relationshipsSum :: ClassConfig -> Maybe Int
relationshipsSum ClassConfig {..} = sumOf4
  <$> snd aggregations
  <*> snd associations
  <*> snd compositions
  <*> snd inheritances
  where
    sumOf4 w x y z = w + x + y + z

maxRels :: ClassConfig -> Int
maxRels config = fromMaybe (maxClasses * (maxClasses - 1) `div` 2)
  $ relationshipsSum config
  where
    maxClasses = snd $ classes config

{-|
Defines the size restrictions of an object diagram.
-}
data ObjectConfig = ObjectConfig {
  -- | lower and upper limit of links within the object diagram
  links             :: !(Int, Maybe Int),
  -- | lower and upper limit of links starting or ending at each object
  linksPerObject    :: !(Int, Maybe Int),
  -- | lower and upper limit of objects within the object diagram
  objects           :: !(Int, Int)
  } deriving (Eq, Generic, Read, Show)

{-|
Defines an 'ObjectConfig' demanding at least one but at most five objects
without restricting links.
-}
maxFiveObjects :: ObjectConfig
maxFiveObjects = ObjectConfig {
  links             = (0, Nothing),
  linksPerObject    = (0, Nothing),
  objects           = (1, 5)
  }

data RelationshipProperties = RelationshipProperties {
    wrongAssocs             :: Int,
    wrongCompositions       :: Int,
    selfRelationships       :: Int,
    selfInheritances        :: Int,
    hasDoubleRelationships  :: Bool,
    hasReverseRelationships :: Bool,
    hasReverseInheritances  :: Bool,
    hasMultipleInheritances :: Bool,
    hasNonTrivialInheritanceCycles :: Bool,
    hasCompositionCycles    :: Bool,
    hasCompositionsPreventingParts :: Bool,
    hasThickEdges           :: Maybe Bool
  } deriving (Generic, Read, Show)

defaultProperties :: RelationshipProperties
defaultProperties = RelationshipProperties {
    wrongAssocs             = 0,
    wrongCompositions       = 0,
    selfRelationships       = 0,
    selfInheritances        = 0,
    hasDoubleRelationships  = False,
    hasReverseRelationships = False,
    hasReverseInheritances  = False,
    hasMultipleInheritances = False,
    hasNonTrivialInheritanceCycles = False,
    hasCompositionCycles    = False,
    hasCompositionsPreventingParts = False,
    hasThickEdges           = Nothing
  }

toOldSyntax :: Syntax -> ([(String, Maybe String)], [Association])
toOldSyntax = first (map $ second listToMaybe)

classNames :: Syntax -> [String]
classNames = map fst . fst

associationNames :: Syntax -> [String]
associationNames = map assocName . snd
  where
    assocName (_, x, _, _, _, _) = x

classNamesOd :: Od -> [String]
classNamesOd o = head . splitOn "$" <$> fst o

linkNames :: Od -> [String]
linkNames o = nub $ (\(_,_,x) -> x) `map` snd o

addedAssociation :: Change DiagramEdge -> Maybe String
addedAssociation c = add c >>= connectionName
  where
    connectionName (_, _, Assoc _ n _ _ _) = Just n
    connectionName (_, _, Inheritance)     = Nothing

renameAssocsInEdge
  :: MonadThrow m
  => Bimap String String
  -> DiagramEdge
  -> m DiagramEdge
renameAssocsInEdge m (f, t, a) = (f, t,) <$> renameConnection a
  where
    renameConnection Inheritance           = return Inheritance
    renameConnection (Assoc ct n lf lt im) = (\n' -> Assoc ct n' lf lt im)
      <$> BM.lookup n m

renameAssocsInCd :: MonadThrow m => Bimap String String -> Syntax -> m Syntax
renameAssocsInCd m cd = (fst cd,) <$> mapM (renameAssocsInAssociation m) (snd cd)

renameAssocsInAssociation
  :: MonadThrow m
  => Bimap String String
  -> Association
  -> m Association
renameAssocsInAssociation m (t, n, fl, fc, tc, tl) = do
  n' <- rename n
  return (t, n', fl, fc, tc, tl)
  where
    rename = (`BM.lookup` m)

renameClassesInEdge
  :: MonadThrow m
  => Bimap String String
  -> DiagramEdge
  -> m DiagramEdge
renameClassesInEdge m (f, t, a) = (,,a) <$> rename f <*> rename t
  where
    rename = (`BM.lookup` m)

renameClassesInCd :: MonadThrow m => Bimap String String -> Syntax -> m Syntax
renameClassesInCd m cd = (,)
  <$> mapM (bimapM rename $ mapM rename) (fst cd)
  <*> mapM (renameClassesInAssociation m) (snd cd)
  where
    rename = (`BM.lookup` m)

renameClassesInAssociation
  :: MonadThrow m
  => Bimap String String
  -> Association
  -> m Association
renameClassesInAssociation m (t, n, fl, fc, tc, tl) = do
  fc' <- rename fc
  tc' <- rename tc
  return (t, n, fl, fc', tc', tl)
  where
    rename = (`BM.lookup` m)

renameLinksInOd :: MonadThrow m => Bimap String String -> Od -> m Od
renameLinksInOd m od = (fst od,) <$> mapM rename (snd od)
  where
    rename (f, t, l) = (f,t,) <$> BM.lookup l m

{-|
Renames all the class names by replacing class names by their new version of
the given mapping.

Object diagrams contain class names within their object names.
The class names, start the object name, they are followed by a @$@ sign.
Therefore renaming those is sufficient when renaming the classes in ODs.
There are no empty object diagram names.
(That is why the non-exhaustive pattern match is safe here.)
-}
renameClassesInOd :: MonadThrow m => Bimap String String -> Od -> m Od
renameClassesInOd m od = (,snd od) <$> mapM (rename . splitOn "$") (fst od)
  where
    rename (l:ls) = (++ '$' : intercalate "$" ls) <$> BM.lookup l m
