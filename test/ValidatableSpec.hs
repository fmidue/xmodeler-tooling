module ValidatableSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty)

import Control.Monad (forM_)

import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.Types (valid)

spec :: Spec
spec = do

  forM_ ["aaaaa.xml"] $ \file ->
      describe "valid" $
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- fromXModeler file
            return $ input `shouldSatisfy` valid ()
  forM_ ([1..71] ++ [73..100] :: [Int]) $ \i ->
      let file = "UML_examples/testing_" ++ show i ++ ".xml" in
      describe "valid" $
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- fromXModeler file
            return $ input `shouldSatisfy` valid ()
  describe "invalid" $
    it ("correctly judges " ++ "UML_examples/testing_72.xml") $
      ioProperty $ do
        input <- fromXModeler "UML_examples/testing_72.xml"
        return $ input `shouldSatisfy` (not . valid ())