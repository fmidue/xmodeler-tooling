module Modelling.CdOd.Populate (populateCd) where

import Modelling.CdOd.Types (AssociationType(Association), ObjectConfig(..))
import Modelling.CdOd.CD2Alloy.Transform (transform, combineParts, createRunCommand)
import Modelling.CdOd.Auxiliary.Util (getInstances, alloyInstanceToOd)

import Data.List ((\\))
import Control.Monad.Trans.Except (runExceptT)

populateCd ::
  Bool
  -> ([(String, Maybe String)], [(String, (Int, Maybe Int), String, String, (Int, Maybe Int))])
  -> [String]
  -> Bool
  -> Int
  -> Int
  -> Int
  -> Bool
  -> Integer
  -> IO [Either String ([String], [(Int, Int, String)])]
populateCd
  noIsolationLimitation
  (classes, associations)
  abstractClasses
  enforceObjects
  numObjectsMin
  numObjectsMax
  numLinksMin
  allowSelfLinks
  number
  = do
      let objectConfig = ObjectConfig {
            links             = (numLinksMin, Nothing),
            linksPerObject    = (0, Nothing),
            objects           = (numObjectsMin, numObjectsMax)
            }
          parts = combineParts $ transform
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
      let alloyCode = parts ++ instantiationConstraint ++ command
      -- putStrLn alloyCode
      let timeout = Nothing
      putStrLn "\nI am letting Alloy do its work now.\n"
      mapM (runExceptT . alloyInstanceToOd) =<< getInstances (Just number) timeout alloyCode
