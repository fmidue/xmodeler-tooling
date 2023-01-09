{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Modelling.MLM.FromXModeler (fromXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.ToXModeler (toXModeler)
import Helpers (spaceOut, scaleFactor, extraOffset, offerChange)
import Test.QuickCheck (frequency, generate, chooseInt)
import Modelling.MLM.Config (Config(..), defaultConfig)
import Modelling.MLM.Edit (Edit(..), editValidly)
import Modelling.MLM.Types (MLM(..), Name(..), Class(..), LeniencyConsideringConcretization(..))
import Modelling.MLM.Validate (valid)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)
import Control.Monad (foldM, when)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import Data.Aeson.KeyMap (KeyMap)
import Data.Yaml (decodeFileThrow)

debug :: Bool
debug = False

requireInstantiations :: LeniencyConsideringConcretization
requireInstantiations = BeLenientAboutConcretization

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [fileName] <- getArgs
  mlm <- fromXModeler fileName
  putStrLn $ "\nIf you want to use the default in any of the following settings, just hit <return> there."
  putStrLn "\nWhich file do you want to use as string dictionary (default is strings.yaml)?"
  dictName <- getLine
  dictionary <- decodeFileThrow $ if null dictName then "strings.yaml" else dictName
  putStrLn "\nShould it be enforced that each existing non-abstract class occurs concretized at least once (default is yes)?"
  enforceClasses <- getLine
  let enforceC = enforceClasses /= "no"
  putStrLn "\nHow many class concretizations do you want me to perform at least (default is 5)?"
  newClasses <- getLine
  let newC = if null newClasses then 5 else read newClasses
  putStrLn "\nHow many links do you want me to add at least (default is 5)?"
  newLinks <- getLine
  let newL = if null newLinks then 5 else read newLinks
  layoutCommand <- offerChange ("(options are Graphviz's " ++ intercalate ", " (map show [minBound .. maxBound :: GraphvizCommand]) ++ ")\nlayoutCommand") Neato
  putStrLn $ "\nJust so that you know: I consider the given MLM to be " ++ (if valid requireInstantiations mlm then "" else "in") ++ "valid.\n"
  let file = "populated_" ++ fileName
  mlm' <- generateAndTest mlm enforceC newC newL dictionary (newC + newL)
  putStrLn $ "\nI am writing the populated MLM to file " ++ file ++ " now.\n"
  export <- toXModeler (layoutCommand, spaceOut, scaleFactor, extraOffset) mlm'{name = Name "populated" }
  writeFile file export

generateAndTest :: MLM -> Bool -> Int -> Int -> KeyMap [String] -> Int -> IO MLM
generateAndTest mlm@MLM{classes, links} enforceClasses newClasses newLinks dictionary = loop
  where
    loop n =
      (\case
          mlm':_
            | length (#classes mlm') >= length classes + newClasses
              && length (#links mlm') >= length links + newLinks
              && (not enforceClasses ||
                   all (\Class{level, isAbstract, name} -> level == 0 || isAbstract
                         || name `elem` mapMaybe #classifier (#classes mlm'))
                   classes)
              -> return mlm'
          _
              -> loop (n+1)
      )
      . dropWhile (not . valid requireInstantiations)
      =<< debugOutput n
      =<< do
          f1 <- generate $ chooseInt (1,20)
          f2 <- generate $ chooseInt (1,20)
          when debug (putStr $ show (f1,f2))
          foldM (\list@(mlm':_) _ ->
                    fmap (:list)
                    . generate $ editValidly requireInstantiations defaultConfig{ tendencyToConcretize = 1.0 } mlm' dictionary
                    =<<
                    -- additional possibilities would be:
                    --   AddAssociation, AddAttribute, AddOperation,
                    --   DeleteClass, DeleteAssociation, DeleteLink,
                    --   DeleteAttribute, DeleteOperation
                    frequency [(f1, return AddClass), (f2, return AddLink)]
                ) [mlm] [1 .. n]

debugOutput :: Int -> [MLM] -> IO [MLM]
debugOutput n list =
  when debug (toXModeler (Neato, spaceOut, scaleFactor, extraOffset) (head list)
              >>= \export -> writeFile (show n ++ ".xml") export
                             >> putStr (' ' : show n ++ "; "))
  >> return list
