module Modelling.CdOd.Populate (populateCdOd) where

import Modelling.CdOd.Types (AssociationType(Association), ObjectConfig(..))
import Modelling.CdOd.CD2Alloy.Transform (transform, combineParts, createRunCommand)
import Modelling.CdOd.Auxiliary.Util (getInstances, alloyInstanceToOd)

import Data.List ((\\))
import Data.Tuple.Extra (both)
import Data.Bifunctor (first)
import Control.Monad.Trans.Except (runExceptT)

populateCdOd ::
  Bool
  -> ([(String, Maybe String)], [(String, (Int, Maybe Int), String, String, (Int, Maybe Int))])
  -> [String]
  -> Bool
  -> [(String, String)]
  -> Int
  -> Int
  -> [(String, String, String)]
  -> Int
  -> Bool
  -> Integer
  -> IO [Either String ([Either String (String, String)], [(Int, Int, String)])]
populateCdOd
  noIsolationLimitation
  (classes, associations)
  abstractClasses
  enforceObjects
  theObjects
  newObjectsMin
  newObjectsMax
  links
  newLinksMin
  allowSelfLinks
  number
  = do
      let objectConfig = ObjectConfig {
            links             = (length links + newLinksMin, Nothing),
            linksPerObject    = (0, Nothing),
            objects           = both (length theObjects +) (newObjectsMin, newObjectsMax)
            }
          parts = transform
            (classes, map (\(a,b,c,d,e) -> (Association,a,b,c,d,e)) associations)
            abstractClasses
            objectConfig
            (if allowSelfLinks then Nothing else Just False)
            noIsolationLimitation
            ""
            ""
          instantiationConstraint
            | not enforceObjects = ""
            | otherwise =
                "fact EnforceObjects {\n" ++ unlines (map ("  some " ++) (map fst classes \\ abstractClasses)) ++ "}\n"
          command = createRunCommand
            "cd"
            (length classes)
            objectConfig
            parts
      let alloyCode =
            combineParts parts
            ++ instantiationConstraint
            ++ unlines (map (\(objectName, className)
                             -> "one sig " ++ objectName ++ " extends " ++ className ++ " {}")
                        theObjects)
            ++ unlines (map (\(sourceName, targetName, associationName)
                             -> "fact { " ++ targetName ++ " in " ++ sourceName ++ ".get[" ++ associationName ++ "] }")
                        links)
            ++ command
      -- putStrLn alloyCode
      let timeout = Nothing
      putStrLn "\nI am letting Alloy do its work now.\n"
      mapM (runExceptT . fmap (first oldOrNew) . alloyInstanceToOd) =<< getInstances (Just number) timeout alloyCode
        where
          oldOrNew =
            map (\objectName ->
                    let (name, rest) = break (=='$') objectName
                    in
                      if name `elem` map fst theObjects
                      then
                        (if rest == "$0" then Left name else error "invalid object name")
                      else
                        Right ("new_" ++ name ++ '_' : show (1 + read (tail rest) :: Int), name)
                )
