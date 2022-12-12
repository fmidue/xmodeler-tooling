{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Modelling.MLM.FromXModeler (fromXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.ToXModeler (toXModeler)
import Helpers (spaceOut, scaleFactor, extraOffset, offerChange)
import Test.QuickCheck (Gen, frequency, generate)
import Modelling.MLM.Config (Config(..), defaultConfig)
import Modelling.MLM.Edit (Edit(..), editValidly)
import Modelling.MLM.Types (MLM(..), Name(..), Class(..))
import Modelling.MLM.Validate (valid)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.List (intercalate)
import Data.Maybe (mapMaybe, listToMaybe)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [fileName] <- getArgs
  mlm <- fromXModeler fileName
  putStrLn "\nShould it be enforced that each existing class occurs concretized at least once (default is yes)?"
  enforceClasses <- getLine
  let enforceC = enforceClasses /= "no"
  putStrLn "\nHow many class concretizations do you want me to perform at least (default is 5)?"
  newClasses <- getLine
  let newC = if null newClasses then 5 else read newClasses
  putStrLn "\nHow many links do you want me to add at least (default is 5)?"
  newLinks <- getLine
  let newL = if null newLinks then 5 else read newLinks
  layoutCommand <- offerChange ("(options are Graphviz's " ++ intercalate ", " (map show [minBound .. maxBound :: GraphvizCommand]) ++ ")\nlayoutCommand") Neato
  let file = "populated-" ++ fileName
  mlm' <- generate $ generateAndTest mlm enforceC newC newL (newC + newL)
  putStrLn $ "\nI am writing the populated MLM to file " ++ file ++ " now.\n"
  export <- toXModeler (layoutCommand, spaceOut, scaleFactor, extraOffset) mlm'{name = Name "populated" }
  writeFile file export

generateAndTest :: MLM -> Bool -> Int -> Int -> Int -> Gen MLM
generateAndTest mlm@MLM{classes, links} enforceClasses newClasses newLinks = loop
  where
    loop n =
      (\case
          Just mlm'
            | length (#classes mlm') >= length classes + newClasses
              && length (#links mlm') >= length links + newLinks
              && (not enforceClasses ||
                   all (\Class{level, name} -> level == 0
                         || name `elem` mapMaybe #classifier (#classes mlm'))
                   classes)
              -> return mlm'
          _
              -> loop (n+1)
      ) . listToMaybe . dropWhile (not . valid False)
      =<< foldM (\list@(mlm':_) _ ->
                    fmap (:list)
                    . editValidly False defaultConfig{ tendencyToConcretize = 1.0 } mlm'
                    =<<
                    -- additional possibilities would be:
                    --   AddAssociation, AddAttribute, AddOperation,
                    --   DeleteClass, DeleteAssociation, DeleteLink,
                    --   DeleteAttribute, DeleteOperation
                    frequency [(1, return AddClass), (10, return AddLink)]
                ) [mlm] [1 .. n]
