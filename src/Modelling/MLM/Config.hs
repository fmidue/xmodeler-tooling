module Modelling.MLM.Config (Config(..)) where

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
  } deriving Show
