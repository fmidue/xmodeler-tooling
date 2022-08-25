module ValidatableSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty)

import Control.Monad (forM_)

import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.Types (valid)

spec :: Spec
spec = do
  forM_ ["exampleInpt1.xml", "exampleInpt2.xml", "exampleInpt3.xml"] $ \file ->
      describe "valid" $ do
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- fromXModeler file
            return $ input `shouldSatisfy` not . valid ()
  forM_ [1..100 :: Int] $ \i ->
      let file = "UML_examples/testing_" ++ show i ++ ".xml" in
      describe "valid" $ do
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- fromXModeler file
            return $ input `shouldSatisfy` valid ()
