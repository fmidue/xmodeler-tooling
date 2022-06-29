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

class XModelerable a where
  get :: a -> String

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
toXModeler = get