module Modelling.MLM.Config (Config(..), defaultConfig) where

import Data.Ratio ((%))

data Config = Config
  { projectNameString :: String
  , maxLvl0 :: Int -- max level of classes
  , numClasses0 :: Int -- number of classes to generate
  , numAssociations0 :: Int -- number of associations to generate
  , chanceToConcretize :: Rational -- the chance for a generated class to be an instance
  , chanceToInherit :: Rational -- the chance for a generated class to have parents
  , multSpecsAttributes :: (Rational, Int) -- (chance for an attribute to have no upper bound, max value of upper bound of a multiplicity of an attribute)
  , multSpecsAssociations :: (Rational, Int) -- same as above, but for associations
  , chanceVisibleAssociation :: Rational -- the chance for an association to have visibility of True
  , chanceAbstractClass :: Rational -- the chance for a class to be abstract
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { projectNameString = "someMLM"
  , maxLvl0 = 5
  , numClasses0 = 30
  , numAssociations0 = 30
  , chanceToConcretize = 1 % 2
  , chanceToInherit = 1 % 2
  , multSpecsAttributes = (1 % 2, 2)
  , multSpecsAssociations = (1 % 2, 2)
  , chanceVisibleAssociation = 1 % 2
  , chanceAbstractClass = 2 % 10
  }
