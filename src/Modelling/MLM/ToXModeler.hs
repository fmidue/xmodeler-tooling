{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

-- remember to export these properly when done testing:
module Modelling.MLM.ToXModeler where

-- remember to import these properly when done testing:
import Data.String.Interpolate (i)
import Data.GraphViz (GraphvizCommand)
import Diagrams.Points (Point (P))
import Diagrams.TwoD.Types (V2 (V2))
import Diagrams.TwoD.GraphViz (layoutGraph, mkGraph, getGraph)
import Data.Map.Strict (toList)
import Modelling.MLM.Types
import Data.Bifunctor (first, second, bimap)

class XModelerable c a where
  get :: c -> a -> String

instance XModelerable MLM where
  get _ = "TODO : THIS SHOULD BE A MULTI-LEVEL MODEL"

instance XModelerable Type where
  get x = let
    cor = "XCore::" ++ show x
    aux = "Auxiliary::" ++ show x
    in
      case x of
        Boolean -> cor
        Integer -> cor
        Float -> cor
        String -> cor
        Element -> cor
        _ -> aux

toXModeler :: MLM -> String
toXModeler = gettoXModeler    (layoutCommand, spaceOut, scaleFactor, extraOffset)
              mlm@(MLM _ mlmClasses mlmAssociations mlmLinks) = let
    vertices = map cName mlmClasses :: [String]
    edges    = map (\x -> (cName (sSource x), cName (sTarget x), ())) mlmAssociations ++
               map (\x -> (cName (lSource x), cName (lTarget x), ())) mlmLinks :: [(String, String, ())]
    adjust :: Double -> Int
    adjust = round . spaceOut
  in do
    g <- layoutGraph layoutCommand $ mkGraph vertices edges
    let objects = map (\(vertex, P (V2 x y)) -> (vertex, (x, y))) $ toList $ fst $ getGraph g :: [(String, (Double, Double))]
    let objects' = map (second (bimap adjust adjust)) objects :: [Object]
    return $ get (objects', scaleFactor, extraOffset) mlm