module Config (reasonableConfigs) where

import Test.QuickCheck (Gen, choose, chooseInt)

import Modelling.MLM.Config (Config(..))

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
