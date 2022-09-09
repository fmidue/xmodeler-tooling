module Modelling.MLM.Config (Config(..), defaultConfig) where

data Config = Config
  { projectNameString :: String
  , maxClassLevel :: Int -- max level of classes
  , numberOfClasses :: Int -- number of classes to generate
  , numberOfAssociations :: Int -- number of associations to generate
  , chanceToConcretize :: Float -- the chance for a generated class to be an instance
  , chanceToInherit :: Float -- the chance for a generated class to have parents
  , multiplicitySpecAttributes :: (Float, Int) -- (chance for an attribute to have an upper bound, max value of upper bound of a multiplicity of an attribute)
  , multiplicitySpecsAssociations :: (Float, Int) -- same as above, but for associations
  , chanceVisibleAssociation :: Float -- the chance for an association to have visibility of True
  , chanceAbstractClass :: Float -- the chance for a class to be abstract
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { projectNameString = "someMLM"
  , maxClassLevel = 5
  , numberOfClasses = 30
  , numberOfAssociations = 30
  , chanceToConcretize = 0.5
  , chanceToInherit = 0.5
  , multiplicitySpecAttributes = (0.5, 2)
  , multiplicitySpecsAssociations = (0.5, 2)
  , chanceVisibleAssociation = 0.5
  , chanceAbstractClass = 0.2
  }
