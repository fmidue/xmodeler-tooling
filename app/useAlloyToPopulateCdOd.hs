{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Modelling.MLM.FromXModeler (fromXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.ToXModeler (toXModeler)
import Modelling.MLM.Types (MLM(..), Class(..), Association(..), Name(..), Multiplicity(..), Link(..), LeniencyConsideringConcretization(..))
import Modelling.MLM.Validate (valid)
import Modelling.MLM.Edit (refreshInstantiationAllClasses)
import Modelling.CdOd.Populate (populateCdOd)

import Test.QuickCheck (generate)

import Helpers (spaceOut, scaleFactor, extraOffset, offerChange)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)
import Data.List (intercalate, partition)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Either (rights)

import Data.Aeson.KeyMap (KeyMap)
import Data.Yaml (decodeFileThrow)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [fileName] <- getArgs
  mlm <- fromXModeler fileName
  isEligible mlm >>= \case
    False ->
      putStrLn " I am not okay with that. Goodbye."
    _ -> do
      putStrLn "\nIf you want to use the default in any of the following settings, just hit <return> there."
      putStrLn "\nWhich file do you want to use as string dictionary (default is strings.yaml)?"
      dictName <- getLine
      dictionary <- decodeFileThrow $ if null dictName then "strings.yaml" else dictName
      putStrLn "\nShould it be enforced that each non-abstract class has at least one object (default is yes)?"
      enforceObjects <- getLine
      let enforceO = enforceObjects /= "no"
      putStrLn "\nHow many objects do you want me to add at least (default is 5)?"
      newObjectsMin <- getLine
      let newOMin = max 0 (if null newObjectsMin then 5 else read newObjectsMin)
      putStrLn "\nHow many objects do you want me to add at most (not smaller than previous number, default is 10)?"
      newObjectsMax <- getLine
      let newOMax = max newOMin (if null newObjectsMax then 10 else read newObjectsMax)
      putStrLn "\nHow many links do you want me to add at least (default is 5)?"
      newLinksMin <- getLine
      let newLMin = max 0 (if null newLinksMin then 5 else read newLinksMin)
      putStrLn "\nShould it be allowed that an object has a link to itself (default is yes)?"
      allowSelfLinks <- getLine
      let allowS = allowSelfLinks /= "no"
      putStrLn "\nHow many populated MLMs do you want me to produce at most (default is 10)?"
      inputN <- getLine
      let n = if null inputN then 10 else read inputN
      layoutCommand <- offerChange ("(options are Graphviz's " ++ intercalate ", " (map show [minBound .. maxBound :: GraphvizCommand]) ++ ")\nlayoutCommand") Neato
      mlms <- makeMLMs mlm enforceO newOMin newOMax newLMin allowS n dictionary
      mapM_ (writeMLM layoutCommand fileName) $ zip [1..] mlms
      putStrLn $ "\nTo my eyes," ++ (if all (valid BeLenientAboutConcretization) mlms then "" else " not") ++ " all of the MLMs produced look valid."

makeMLMs :: MLM -> Bool -> Int -> Int -> Int -> Bool -> Integer -> KeyMap [String] -> IO [MLM]
makeMLMs mlm@MLM{classes, associations, links} enforceObjects newObjectsMin newObjectsMax newLinksMin allowSelfLinks number dictionary =
  let
    (theClasses, theObjects) = partition ((1==) . #level) classes
    noIsolationLimitation = True
    cd = ( map (\Class{name = Name className, parents} ->
                   (className,
                    (\(Name parentName) -> parentName) <$> listToMaybe parents)
               ) theClasses
         , map (\Association{name = Name associationName, source = Name sourceName, target = Name targetName, multSource = Multiplicity sourceMult, multTarget = Multiplicity targetMult} ->
                   ( associationName
                   , sourceMult, sourceName
                   , targetName, targetMult
                   )
               ) associations
         )
    abstractClasses = mapMaybe (\Class{isAbstract, name = Name className} -> if isAbstract then Just className else Nothing) theClasses
  in
    populateCdOd
    noIsolationLimitation
    cd
    abstractClasses
    enforceObjects
    (map (\Class{name = Name objectName, classifier = Just (Name className)}
          -> (objectName, className)) theObjects)
    newObjectsMin
    newObjectsMax
    (map (\Link{association = Name associationName, source = Name sourceName, target = Name targetName}
          -> (sourceName, targetName, associationName)) links)
    newLinksMin
    allowSelfLinks
    number
    >>= mapM (
    \(Right (objects, linkList)) ->
      generate . refreshInstantiationAllClasses dictionary $
      mlm{ classes = classes ++ map (\(object, itsClass) ->
                                       Class{ isAbstract = False
                                            , level = 0
                                            , name = Name object
                                            , parents = []
                                            , classifier = Just (Name itsClass)
                                            , attributes = []
                                            , operations = []
                                            , slots = []}
                                    ) (rights objects)
         , links = map (\(s,t,n) ->
                          Link{ association = Name n
                              , source = Name (either id fst (objects !! s))
                              , target = Name (either id fst (objects !! t))
                              }
                       ) linkList
         }
    )

isEligible :: MLM -> IO Bool
isEligible mlm@MLM{classes, associations}
  | null classes =
      putStr "\nThere are no classes in the given MLM."
      >> return False
  | any (\Class{level} -> level > 1) classes =
      putStr "\nThere are classes at levels higher than 1 in the given MLM."
      >> return False
  | null associations =
      putStr "\nThere are no associations in the given MLM. That is boring."
      >> return False
  | any (\Class{parents} -> length parents > 1) classes =
      putStr "\nThere is multiple inheritance in the given MLM."
      >> return False
  | not (valid BeLenientAboutConcretization mlm) =
      putStr "\nThe given MLM does not look valid."
      >> return False
  | otherwise =
      putStrLn "\nJust so that you know: I consider the given MLM to be valid and eligible for what I am trying to do."
      >> return True

writeMLM :: GraphvizCommand -> String -> (Int, MLM) -> IO ()
writeMLM layoutCommand fileName (i, mlm) = do
  let file = "populated_" ++ show i ++ "_" ++ fileName
  putStrLn $ "\nI am writing the populated MLM #" ++ show i ++ " to file " ++ file ++ " now.\n"
  export <- toXModeler (layoutCommand, spaceOut, scaleFactor, extraOffset) mlm
  writeFile file export
