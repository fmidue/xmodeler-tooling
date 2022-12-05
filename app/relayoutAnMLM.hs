module Main (main) where

import Modelling.MLM.FromXModeler (fromXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.ToXModeler (toXModeler)
import Helpers (spaceOut, scaleFactor, extraOffset, offerChange)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)
import Data.List (intercalate)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [fileName] <- getArgs
  mlm <- fromXModeler fileName
  layoutCommand <- offerChange ("(options are Graphviz's " ++ intercalate ", " (map show [minBound .. maxBound :: GraphvizCommand]) ++ ")\nlayoutCommand") Neato
  let file = show layoutCommand ++ "-" ++ fileName
  putStrLn $ "\nI am writing the relayouted MLM to file " ++ file ++ " now.\n"
  export <- toXModeler (layoutCommand, spaceOut, scaleFactor, extraOffset) mlm
  writeFile file export
