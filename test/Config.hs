module Config (reasonableConfigs, smallConfigs) where

import Test.QuickCheck (Gen, choose, chooseInt)

import Modelling.MLM.Config (Config(..))

randomMultSpec :: Gen (Float, Int)
randomMultSpec = do
  rc <- choose (0.0, 1.0) :: Gen Float
  m <- chooseInt (1,3) :: Gen Int
  return (rc, m)

reasonableConfigs :: Gen Config
reasonableConfigs = do
  let projectNameString = "randomMLM"
  maxClassLevel <- choose (3,7)
  numberOfClasses <- choose (25,35) -- I advice to not surpass 35 classes, because XModeler struggles to handle that (at least on my computer).
  numberOfAssociations <- choose (25,35)
  chanceToConcretize <- choose (0.0, 1.0)
  chanceToInherit <- choose (0.0, 1.0)
  multiplicitySpecAssociations <- randomMultSpec
  chanceVisibleAssociation <- choose (0.0, 1.0)
  chanceAbstractClass <- choose (0.0, 1.0)
  portionOfPossibleLinksToKeep <- choose (0.0, 1.0)
  averageNumberOfAttributesPerClass <- choose (0,6)
  tendencyToDistanceAttributeFromItsInstantiation <- choose (0.0, 1.0)
  return $ Config {..}

smallConfigs :: Gen Config
smallConfigs = do
  let projectNameString = "randomMLM"
  maxClassLevel <- choose (1,4)
  numberOfClasses <- choose (5,12)
  numberOfAssociations <- choose (5,8)
  chanceToConcretize <- choose (0.4, 1.0)
  chanceToInherit <- choose (0.0, 0.5)
  multiplicitySpecAssociations <- do
    rc <- choose (0.0, 1.0) :: Gen Float
    m <- chooseInt (1,3) :: Gen Int
    return (rc, m)
  let chanceVisibleAssociation  = 0.0
  let chanceAbstractClass = 0.0
  portionOfPossibleLinksToKeep <- choose (0.7, 1.0)
  averageNumberOfAttributesPerClass <- choose (1,2)
  tendencyToDistanceAttributeFromItsInstantiation <- choose (0.0, 0.2)
  return $ Config {..}