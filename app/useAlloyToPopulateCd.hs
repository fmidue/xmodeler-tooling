{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Modelling.MLM.FromXModeler (fromXModeler)
import Data.GraphViz (GraphvizCommand(..))
import Modelling.MLM.ToXModeler (toXModeler)
import Modelling.MLM.Types (MLM(..), Class(..), Association(..), Name(..), Multiplicity(..), Link(..))
import Modelling.MLM.Validate (valid)
import Modelling.MLM.Edit (refreshInstantiationAllClasses)
import Modelling.CdOd.Populate (populateCd)

import Test.QuickCheck (generate)

import Helpers (spaceOut, scaleFactor, extraOffset, offerChange)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.List.Extra (replace)
import Data.Maybe (mapMaybe, listToMaybe)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [fileName] <- getArgs
  mlm <- fromXModeler fileName
  isEligible mlm >>= \case
    False ->
      putStrLn " I am not okay with that. Goodbye."
    _ -> do
      putStrLn "\nShould it be enforced that each non-abstract class is instantiated at least once (default is yes)?"
      enforceObjects <- getLine
      let enforceO = enforceObjects /= "no"
      putStrLn "\nHow many objects do you want me to create at least (minimum is 1, default is 5)?"
      numObjectsMin <- getLine
      let numOMin = if null numObjectsMin then 5 else read numObjectsMin
      putStrLn "\nHow many objects do you want me to create at most (default is 10)?"
      numObjectsMax <- getLine
      let numOMax = if null numObjectsMax then 10 else read numObjectsMax
      putStrLn "\nHow many links do you want me to create at least (default is 5)?"
      numLinksMin <- getLine
      let numLMin = if null numLinksMin then 5 else read numLinksMin
      putStrLn "\nShould it be allowed that an object has a link to itself (default is yes)?"
      allowSelfLinks <- getLine
      let allowS = allowSelfLinks /= "no"
      putStrLn "\nHow many populated MLMs do you want me to produce at most (default is 10)?"
      inputN <- getLine
      let n = if null inputN then 10 else read inputN
      layoutCommand <- offerChange ("(options are Graphviz's " ++ intercalate ", " (map show [minBound .. maxBound :: GraphvizCommand]) ++ ")\nlayoutCommand") Neato
      mlms <- makeMLMs mlm enforceO numOMin numOMax numLMin allowS n
      mapM_ (writeMLM layoutCommand fileName) $ zip [1..] mlms
      putStrLn $ "\nTo my eyes," ++ (if all (valid False) mlms then "" else " not") ++ " all of the MLMs produced look valid."

makeMLMs :: MLM -> Bool -> Int -> Int -> Int -> Bool -> Integer -> IO [MLM]
makeMLMs mlm@MLM{classes, associations} enforceObjects numObjectsMin numObjectsMax numLinksMin allowSelfLinks number =
  let
    noIsolationLimitation = True
    cd = ( map (\Class{name = Name className, parents} ->
                   (className,
                    (\(Name parentName) -> parentName) <$> listToMaybe parents)
               ) classes
         , map (\Association{name = Name associationName, source = Name sourceName, target = Name targetName, multSource = Multiplicity sourceMult, multTarget = Multiplicity targetMult} ->
                   ( associationName
                   , sourceMult, sourceName
                   , targetName, targetMult
                   )
               ) associations
         )
    abstractClasses = mapMaybe (\Class{isAbstract, name = Name className} -> if isAbstract then Just className else Nothing) classes
  in
    populateCd noIsolationLimitation cd abstractClasses enforceObjects numObjectsMin numObjectsMax numLinksMin allowSelfLinks number
    >>= mapM (
    \(Right (objectNames, linkList)) ->
      generate . refreshInstantiationAllClasses $
      mlm{ classes = classes ++ map (\object ->
                                       Class{ isAbstract = False
                                            , level = 0
                                            , name = Name (replace "$" "_" object)
                                            , parents = []
                                            , classifier = Just (Name (takeWhile (/='$') object))
                                            , attributes = []
                                            , operations = []
                                            , slots = []}
                                    ) objectNames
         , links = map (\(s,t,n) ->
                          Link{ association = Name n
                              , source = Name (replace "$" "_" (objectNames !! s))
                              , target = Name (replace "$" "_" (objectNames !! t))
                              }
                       ) linkList
         }
    )

isEligible :: MLM -> IO Bool
isEligible mlm@MLM{classes, associations}
  | null classes =
      putStr "\nThere are no classes in the given MLM."
      >> return False
  | any (\Class{level} -> level /= 1) classes =
      putStr "\nThere are classes/objects at other levels than 1 in the given MLM."
      >> return False
  | null associations =
      putStr "\nThere are no associations in the given MLM. That is boring."
      >> return False
  | any (\Class{parents} -> length parents > 1) classes =
      putStr "\nThere is multiple inheritance in the given MLM."
      >> return False
  | not (valid True mlm) =
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
