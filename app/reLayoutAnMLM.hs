module Main (main) where

import Modelling.MLM.FromXModeler (fromXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.ToXModeler (toXModeler)
import Modelling.MLM.Validate (valid)

import Helpers (spaceOut, scaleFactor, extraOffset, offerChange)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)
import Data.List (intercalate)

requireInstantiations :: Bool
requireInstantiations = False

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [fileName] <- getArgs
  mlm <- fromXModeler fileName
  layoutCommand <- offerChange ("(options are Graphviz's " ++ intercalate ", " (map show [minBound .. maxBound :: GraphvizCommand]) ++ ")\nlayoutCommand") Neato
  putStrLn $ "\nJust so that you know: I consider the given MLM to be " ++ if valid requireInstantiations mlm then "valid." else "invalid."
  let file = show layoutCommand ++ "_" ++ fileName
  putStrLn $ "\nI am writing the re-layouted MLM to file " ++ file ++ " now.\n"
  export <- toXModeler (layoutCommand, spaceOut, scaleFactor, extraOffset) mlm
  writeFile file export
