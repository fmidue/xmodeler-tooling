--{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Modelling.MLM.ToXModeler where

-- remember to import these properly when done testing:
import Modelling.MLM.Types

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