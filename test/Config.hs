module Config (Config(..), reasonableConfigs) where

import Test.QuickCheck (Gen, choose, elements)

data Config = Config
  { projectNameString :: String
  , maxLvl0 :: Int
  , numClasses0 :: Int
  , numAssociations0 :: Int
  , chanceToNotConcretize :: Int
  , chanceToNotInherit :: Int
  , numAttributes0 :: Int
  , multSpecsAttributes0 :: (Int, Int)
  , precisionFactorAttributes0 :: Int
  , multSpecsAssociations0 :: (Int, Int)
  , visibilityChanceAssociations :: Int
  } deriving Show

reasonableConfigs :: Gen Config
reasonableConfigs = do
  let projectNameString = "randomMLM"
  maxLvl0 <- choose (3,7)
  numClasses0 <- choose (30,40)
  numAssociations0 <- choose (25,35)
  chanceToNotConcretize <- choose (1,5)
  chanceToNotInherit <- choose (0,3)
  numAttributes0 <- choose (50,90)
  multSpecsAttributes0 <- elements [ (a,b) | a <- [2..7], b <- [a..8]]
  precisionFactorAttributes0 <- choose (100,10000)
  multSpecsAssociations0 <- elements [ (a,b) | a <- [0..5], b <- [a..5]]
  visibilityChanceAssociations <- choose (1,10)
  return $ Config {..}
