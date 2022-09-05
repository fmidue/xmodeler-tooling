module Config (Config(..), reasonableConfigs) where

import Test.QuickCheck (Gen, choose, elements)

data Config = Config
  {projectNameString :: String
  , maxLvl0 :: Int -- max level of classes
  , numClasses0 :: Int -- number of classes to generate
  , numAssociations0 :: Int -- number of associations to generate
  , chanceToConcretize :: Float -- the chance for a generated class to be an instance
  , chanceToInherit :: Float -- the chance for a generated class to have parents
  , multSpecsAttributes :: (Float, Int) -- (chance for an attribute to have no upper bound, max value of upper bound of a multiplicity of an attribute)
  , multSpecsAssociations :: (Float, Int) -- same as above, but for associations
  , chanceVisibleAssociation ::Float -- the chance for an association to have visibility of True
  } deriving Show

toFloat :: Int -> Float
toFloat = fromIntegral

reasonableConfigs :: Gen Config
reasonableConfigs = do
  let projectNameString = "randomMLM"
  maxLvl0 <- choose (3,7)
  numClasses0 <- choose (25,35) -- I advice to not surpass 35 classes, because XModeler struggles to handle that (at least on my computer).
  numAssociations0 <- choose (25,35)
  chanceToConcretize <- toFloat <$> choose (0,100)
  chanceToInherit <- toFloat <$> choose (0,100)
  multSpecsAttributes <- elements [(toFloat a,b) | a <- [0..100], b <- [1..3]]
  multSpecsAssociations <- elements [ (toFloat a,b) | a <- [0..100], b <- [1..5]]
  chanceVisibleAssociation <- toFloat <$> choose (0,100)
  return $ Config {..}
