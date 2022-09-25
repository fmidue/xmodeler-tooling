{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Modelling.MLM.ToXModeler (toXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.Config (Config(..), defaultConfig)
import Modelling.MLM.Types (MLM (..), Name (..))
import Modelling.MLM.GenerateMLM (generateMLM)
import Test.QuickCheck (generate)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

spaceOut :: Double -> Double
spaceOut = (**1.2)

scaleFactor :: Double
scaleFactor = 1.1

-- this is the width of a rectangle of a class in XModeler
extraOffset :: Int
extraOffset = 163

layoutCommand :: GraphvizCommand
layoutCommand = Neato

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random MLM generated from it:\n"
  mlm@MLM{ name = Name projectName } <- generate . generateMLM $ theConfigToUse
  pPrint mlm
  putStrLn $ "\nI am also writing the random MLM generated above to file " ++ projectName ++ ".xml now.\n"
  export <- toXModeler (layoutCommand, spaceOut, scaleFactor, extraOffset) mlm
  writeFile (projectName ++ ".xml") export

determineConfig :: IO Config
determineConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultConfig
  let Config{..} = defaultConfig
  projectNameString' <- offerChange "projectNameString" projectNameString
  maxClassLevel' <- offerChange "maxClassLevel" maxClassLevel
  numberOfClasses' <- offerChange "numberOfClasses" numberOfClasses
  numberOfAssociations' <- offerChange "numberOfAssociations" numberOfAssociations
  tendencyToConcretize' <- offerChange "chanceToConcretize" tendencyToConcretize
  tendencyToInherit' <- offerChange "chanceToInherit" tendencyToInherit
  multiplicitySpecAssociations' <- offerChange "multiplicitySpecAssociations" multiplicitySpecAssociations
  chanceVisibleAssociation' <- offerChange "chanceVisibleAssociation" chanceVisibleAssociation
  tendencyAbstractClass' <- offerChange "chanceAbstractClass" tendencyAbstractClass
  portionOfPossibleLinksToKeep' <- offerChange "portionOfPossibleLinksToKeep" portionOfPossibleLinksToKeep
  numberOfAttributesPerConcretization' <- offerChange "numberOfAttributesPerConcretization" numberOfAttributesPerConcretization
  tendencyToDistanceAttributeFromItsInstantiation' <- offerChange "tendencyToDistanceAttributeFromItsInstantiation" tendencyToDistanceAttributeFromItsInstantiation
  allowMultipleInheritance' <- offerChange "allowMultipleInheritance" allowMultipleInheritance
  let newConfig = Config
        { projectNameString = projectNameString'
        , maxClassLevel = maxClassLevel'
        , numberOfClasses = numberOfClasses'
        , numberOfAssociations = numberOfAssociations'
        , tendencyToConcretize = tendencyToConcretize'
        , tendencyToInherit = tendencyToInherit'
        , multiplicitySpecAssociations = multiplicitySpecAssociations'
        , chanceVisibleAssociation = chanceVisibleAssociation'
        , tendencyAbstractClass = tendencyAbstractClass'
        , portionOfPossibleLinksToKeep = portionOfPossibleLinksToKeep'
        , numberOfAttributesPerConcretization = numberOfAttributesPerConcretization'
        , tendencyToDistanceAttributeFromItsInstantiation = tendencyToDistanceAttributeFromItsInstantiation'
        , allowMultipleInheritance = allowMultipleInheritance'
        }
  return newConfig

offerChange :: (Show a, Read a) => String -> a -> IO a
offerChange name value = do
  putStrLn $ "\nIf you want to change the setting " ++ name ++ " = " ++ show value ++ ", enter a new value here (otherwise just hit return):"
  input <- getLine
  if null input
    then return value
    else return (read input)
