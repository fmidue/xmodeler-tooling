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
  maxLvl0' <- offerChange "maxLvl0" maxLvl0
  numClasses0' <- offerChange "numClasses0" numClasses0
  numAssociations0' <- offerChange "numAssociations0" numAssociations0
  chanceToConcretize' <- offerChange "chanceToConcretize" chanceToConcretize
  chanceToInherit' <- offerChange "chanceToInherit" chanceToInherit
  multSpecsAttributes' <- offerChange "multSpecsAttributes" multSpecsAttributes
  multSpecsAssociations' <- offerChange "multSpecsAssociations" multSpecsAssociations
  chanceVisibleAssociation' <- offerChange "chanceVisibleAssociation" chanceVisibleAssociation
  let newConfig = Config
        { projectNameString = projectNameString'
        , maxLvl0 = maxLvl0'
        , numClasses0 = numClasses0'
        , numAssociations0 = numAssociations0'
        , chanceToConcretize = chanceToConcretize'
        , chanceToInherit = chanceToInherit'
        , multSpecsAttributes = multSpecsAttributes'
        , multSpecsAssociations = multSpecsAssociations'
        , chanceVisibleAssociation = chanceVisibleAssociation'
        }
  return newConfig

offerChange :: (Show a, Read a) => String -> a -> IO a
offerChange name value = do
  putStrLn $ "\nIf you want to change the setting " ++ name ++ " = " ++ show value ++ ", enter a new value here (otherwise just hit return):"
  input <- getLine
  if null input
    then return value
    else return (read input)