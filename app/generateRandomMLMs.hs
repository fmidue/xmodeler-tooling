{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Modelling.MLM.ToXModeler (toXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.Config (Config(..), defaultConfig, checkConfig)
import Modelling.MLM.Types (MLM (..), Name (..))
import Modelling.MLM.Generate (generateMLM)
import Test.QuickCheck (generate)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)
import Data.List (intercalate)
import Control.Monad (when)

spaceOut :: Double -> Double
spaceOut = (**1.2)

scaleFactor :: Double
scaleFactor = 1.1

-- this is the width of a rectangle of a class in XModeler
extraOffset :: Int
extraOffset = 163

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nHow many random MLMs do you want me to generate from it (default is 1)?"
  inputN <- getLine
  let n = if null inputN then 1 else read inputN
  putStrLn "\nDo you want me to print Haskell representations of the MLMs (default is no)?"
  inputP <- getLine
  layoutCommand <- offerChange ("(options are Graphviz's " ++ intercalate ", " (map show [minBound .. maxBound :: GraphvizCommand]) ++ ")\nlayoutCommand") Neato
  mapM_ (makeMLM theConfigToUse layoutCommand (inputP == "yes")) [1..n]

makeMLM :: Config -> GraphvizCommand -> Bool -> Int -> IO ()
makeMLM config layoutCommand p i = do
  mlm@MLM{ name = Name projectName } <- generate . generateMLM $ config
  when p $ do
    putStrLn $ "\nThe following is random MLM #" ++ show i ++ " generated from the config:\n"
    pPrint mlm
  let file = projectName ++ show i ++ ".xml"
  putStrLn $ "\nI am " ++
    (if p
      then "also writing the random MLM generated above"
      else "writing random MLM #" ++ show i ++ " generated from the config")
    ++ " to file " ++ file ++ " now.\n"
  export <- toXModeler (layoutCommand, spaceOut, scaleFactor, extraOffset) mlm
  writeFile file export

determineConfig :: IO Config
determineConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultConfig
  let Config{..} = defaultConfig
  projectNameString' <- offerChange "projectNameString" projectNameString
  maxClassLevel' <- offerChange "maxClassLevel" maxClassLevel
  numberOfClasses' <- offerChange "numberOfClasses" numberOfClasses
  numberOfAssociations' <- offerChange "numberOfAssociations" numberOfAssociations
  tendencyToConcretize' <- offerChange "tendencyToConcretize" tendencyToConcretize
  tendencyToInherit' <- offerChange "tendencyToInherit" tendencyToInherit
  multiplicitySpecAssociations' <- offerChange "multiplicitySpecAssociations" multiplicitySpecAssociations
  chanceVisibleAssociation' <- offerChange "chanceVisibleAssociation" chanceVisibleAssociation
  tendencyAbstractClass' <- offerChange "tendencyAbstractClass" tendencyAbstractClass
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
  case checkConfig newConfig of
    Nothing ->
      return newConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineConfig

offerChange :: (Show a, Read a) => String -> a -> IO a
offerChange name value = do
  putStrLn $ "\nIf you want to change the setting " ++ name ++ " = " ++ show value ++ ", enter a new value here (otherwise just hit return):"
  input <- getLine
  if null input
    then return value
    else return (read input)
