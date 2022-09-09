module Modelling.MLM.Config (Config(..), defaultConfig) where

import Data.Ratio ((%))

data Config = Config
  { projectNameString :: String
  , chanceToConcretize :: Rational -- the chance for a generated class to be an instance
  , chanceToInherit :: Rational -- the chance for a generated class to have parents
  , multSpecsAttributes :: (Rational, Int) -- (chance for an attribute to have no upper bound, max value of upper bound of a multiplicity of an attribute)
  , multSpecsAssociations :: (Rational, Int) -- same as above, but for associations
  , chanceVisibleAssociation :: Rational -- the chance for an association to have visibility of True
  , chanceAbstractClass :: Rational -- the chance for a class to be abstract
  , maxClassLevel :: Int -- max level of classes
  , numberOfClasses :: Int -- number of classes to generate
  , numberOfAssociations :: Int -- number of associations to generate
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { projectNameString = "someMLM"
  , chanceToConcretize = 1 % 2
  , chanceToInherit = 1 % 2
  , multSpecsAttributes = (1 % 2, 2)
  , multSpecsAssociations = (1 % 2, 2)
  , chanceVisibleAssociation = 1 % 2
  , chanceAbstractClass = 2 % 10
  , maxClassLevel = 5
  , numberOfClasses = 30
  , numberOfAssociations = 30
  }
