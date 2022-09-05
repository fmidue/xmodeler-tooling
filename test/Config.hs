module Config (Config(..), reasonableConfigs) where

import Test.QuickCheck (Gen, choose, chooseInt)

data Config = Config
  {projectNameString :: String
  , maxLvl0 :: Int -- max level of classes
  , numClasses0 :: Int -- number of classes to generate
  , numAssociations0 :: Int -- number of associations to generate
  , chanceToConcretize :: Rational -- the chance for a generated class to be an instance
  , chanceToInherit :: Rational -- the chance for a generated class to have parents
  , multSpecsAttributes :: (Rational, Int) -- (chance for an attribute to have no upper bound, max value of upper bound of a multiplicity of an attribute)
  , multSpecsAssociations :: (Rational, Int) -- same as above, but for associations
  , chanceVisibleAssociation :: Rational -- the chance for an association to have visibility of True
  } deriving Show

randomChance :: Float -> Float -> Gen Rational
randomChance a b = toRational <$> choose (a, b)

randomChanceStandard :: Gen Rational
randomChanceStandard = randomChance 0.0 1.0

randomMultSpec :: Gen (Rational, Int)
randomMultSpec = do
  rc <- randomChanceStandard :: Gen Rational
  m <- chooseInt (1,3) :: Gen Int
  return (rc, m)

reasonableConfigs :: Gen Config
reasonableConfigs = do
  let projectNameString = "randomMLM"
  maxLvl0 <- choose (3,7)
  numClasses0 <- choose (25,35) -- I advice to not surpass 35 classes, because XModeler struggles to handle that (at least on my computer).
  numAssociations0 <- choose (25,35)
  chanceToConcretize <- randomChanceStandard
  chanceToInherit <- randomChanceStandard
  multSpecsAttributes <- randomMultSpec
  multSpecsAssociations <- randomMultSpec
  chanceVisibleAssociation <- randomChanceStandard
  return $ Config {..}
