module Main (main) where

import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.Validate (valid)
import Modelling.MLM.Types
  ( LeniencyConsideringConcretization(..)
  , LeniencyConsideringSlotFilling(..)
  , LeniencyConsideringLowerMultiplicities(..)
  )

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

requireInstantiations :: LeniencyConsideringConcretization
requireInstantiations = BeLenientAboutConcretization

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [fileName] <- getArgs
  mlm <- fromXModeler fileName
  putStrLn "\nThe following is the MLM imported from the XModeler file:\n"
  pPrint mlm
  putStrLn $ "\nIt is considered "
    ++ (if valid
         ( requireInstantiations
         , BeStrictAboutSlotFilling
         , BeStrictAboutLowerMultiplicities
         )
         mlm
        then "" else "in")
    ++ "valid.\n"
