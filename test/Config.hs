module Config (Config(..), reasonableConfigs) where

import Test.QuickCheck (Gen, choose, elements)

data Config = Config
  { projectNameString :: String
  , maxLvl0 :: Int -- max level of classes
  , numClasses0 :: Int -- number of classes to generate
  , numAssociations0 :: Int -- number of associations to generate
  , chanceToNotConcretize :: Int -- the chance for a generated class to be a metaclass
  , chanceToNotInherit :: Int -- the chance for a generated class to not have parents
  , multSpecsAttributes0 :: (Int, Int) -- (max value of upper bound of a multiplicity of an attribute, chance for an attribute to have no upper bound)
  , multSpecsAssociations0 :: (Int, Int) -- same as above, but for associations
  , visibilityChanceAssociations :: Int -- the chance for an association to have visibility of False
  } deriving Show

-- Notice that the chances are expressed in Int. The point is to allow any Int input to be valid. Every Int value gets converted into a percentage. So, 9 becomes 9/10, 0 becomes 0, 1 becomes 1/10, 10 becomes 10/100 = 1/10, 123 becomes 123/1000, etc.


reasonableConfigs :: Gen Config
reasonableConfigs = do
  let projectNameString = "randomMLM"
  maxLvl0 <- choose (3,7)
  numClasses0 <- choose (30,40) -- I advice to not surpass 35 classes, because XModeler struggles to handle that (at least on my computer).
  numAssociations0 <- choose (25,35)
  chanceToNotConcretize <- choose (1,5)
  chanceToNotInherit <- choose (0,3)
  multSpecsAttributes0 <- elements [ (a,b) | a <- [2..7], b <- [a..8]]
  multSpecsAssociations0 <- elements [ (a,b) | a <- [0..5], b <- [a..5]]
  visibilityChanceAssociations <- choose (1,10)
  return $ Config {..}
