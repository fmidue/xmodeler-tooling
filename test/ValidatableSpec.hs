module ValidatableSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty)

import Data.List ((\\))
import Control.Monad (forM_, forM)
import System.Directory (listDirectory)

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
  forM_ ([1..2292] \\ withIsolatedObjects) $ \i ->
      let file = "UML_examples/testing_" ++ show i ++ ".xml" in
      describe "valid" $
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- fromXModeler file
            return $ input `shouldSatisfy` valid ()
  forM_ withIsolatedObjects $ \i ->
      let file = "UML_examples/testing_" ++ show i ++ ".xml" in
      describe "invalid" $
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- fromXModeler file
            return $ input `shouldSatisfy` (not . valid ())
  describe "invalid" $
    let dir = "should_fail/" in
    it ("correctly judges content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir ++) <$> listDirectory dir
        validities <- forM files $ \file -> ((file,) . valid ()) <$> fromXModeler file
        return $ filter snd validities `shouldSatisfy` null
  describe "valid" $
    let dir = "should_narrowly_pass/" in
    it ("correctly judges content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir ++) <$> listDirectory dir
        validities <- forM files $ \file -> ((file,) . valid ()) <$> fromXModeler file
        return $ filter (not . snd) validities `shouldSatisfy` null

withIsolatedObjects :: [Int]
withIsolatedObjects = [72,220,282,562,582,602,676,734,748,872,882,890,984,1130,1172,1244,1312,1340,1374,1546,1594,1633,1685,1711,1820,1884,1984,2063,2264]
