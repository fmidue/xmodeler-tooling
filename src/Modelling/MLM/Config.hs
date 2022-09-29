{-# LANGUAGE RecordWildCards #-}

module Modelling.MLM.Config (Config(..), defaultConfig, checkConfig) where

import Modelling.MLM.Types (valid, Name(..))

data Config = Config
  { projectNameString :: String
  , maxClassLevel :: Int -- max level of classes
  , numberOfClasses :: Int -- number of classes to generate
  , numberOfAssociations :: Int -- number of associations to generate
  , tendencyToConcretize :: Float -- the chance for a generated class to be an instance
  , tendencyToInherit :: Float -- the chance for a generated class to have parents
  , multiplicitySpecAssociations :: (Float, Int) -- (chance for an association to have an upper bound, max value of upper bound of a multiplicity of an association)
  , chanceVisibleAssociation :: Float -- the chance for an association to have visibility of True
  , tendencyAbstractClass :: Float -- the chance for a class to be abstract
  , portionOfPossibleLinksToKeep :: Float
  , numberOfAttributesPerConcretization :: Int
  , tendencyToDistanceAttributeFromItsInstantiation :: Float
  , allowMultipleInheritance :: Bool
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { projectNameString = "someMLM"
  , maxClassLevel = 5
  , numberOfClasses = 30
  , numberOfAssociations = 30
  , tendencyToConcretize = 0.5
  , tendencyToInherit = 0.5
  , multiplicitySpecAssociations = (0.5, 2)
  , chanceVisibleAssociation = 0.5
  , tendencyAbstractClass = 0.2
  , portionOfPossibleLinksToKeep = 0.8
  , numberOfAttributesPerConcretization = 2
  , tendencyToDistanceAttributeFromItsInstantiation = 0.25
  , allowMultipleInheritance = False
  }

checkConfig :: Config -> Maybe String
checkConfig Config {..}
    | maxClassLevel < 1
      = Just "Cannot have only \"classes\" of level 0 (instances)."
    | numberOfClasses < 1
      = Just "The number of classes must be positive."
    | numberOfAssociations < 0
      = Just "The number of associations cannot be negative."
    | numberOfAttributesPerConcretization < 1
      = Just "At least one attribute per concretization is required."
    | any (\x -> x < 0.0 || x > 1.0) [tendencyToConcretize, tendencyToInherit, fst multiplicitySpecAssociations, chanceVisibleAssociation, tendencyAbstractClass, portionOfPossibleLinksToKeep, tendencyToDistanceAttributeFromItsInstantiation]
      = Just "Every value which represents a chance/tendency/portion must be in the inclusive range (0,1)."
    | snd multiplicitySpecAssociations < 1
      = Just "The upper bound of a multiplicity of an association cannot be less than 1."
    | not (valid () (Name projectNameString))
      = Just "A valid name for a multi-level model cannot be empty and its first character must be a letter (capital or small) and the rest of the characters must be combinations of letters, digits, and underscore ( _ ), but nothing else."
    | otherwise = Nothing
