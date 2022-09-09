module Config (reasonableConfigs) where

import Test.QuickCheck (Gen, choose, chooseInt)

import Modelling.MLM.Config (Config(..))

randomChanceStandard :: Gen Float
randomChanceStandard = choose (0.0, 1.0)

randomMultSpec :: Gen (Float, Int)
randomMultSpec = do
  rc <- randomChanceStandard :: Gen Float
  m <- chooseInt (1,3) :: Gen Int
  return (rc, m)

reasonableConfigs :: Gen Config
reasonableConfigs = do
  let projectNameString = "randomMLM"
  maxClassLevel <- choose (3,7)
  numberOfClasses <- choose (25,35) -- I advice to not surpass 35 classes, because XModeler struggles to handle that (at least on my computer).
  numberOfAssociations <- choose (25,35)
  chanceToConcretize <- randomChanceStandard
  chanceToInherit <- randomChanceStandard
  multiplicitySpecAttributes <- randomMultSpec
  multiplicitySpecsAssociations <- randomMultSpec
  chanceVisibleAssociation <- randomChanceStandard
  chanceAbstractClass <- randomChanceStandard
  return $ Config {..}
