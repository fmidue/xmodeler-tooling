module ValidatableSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty)

import Data.List ((\\))
import Control.Monad (forM_, forM)
import System.Directory (listDirectory)
import System.FilePath (takeExtension)

import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.Types (valid, MLM)

spec :: Spec
spec = do

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
   let dir = "should_fail/" in do
    it ("correctly judges .xml content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir ++) . filter ((".xml" ==) . takeExtension) <$> listDirectory dir
        validities <- forM files $ \file -> (file,) . valid () <$> fromXModeler file
        return $ filter snd validities `shouldSatisfy` null
    it ("correctly judges .hs content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir ++) . filter ((".hs" ==) . takeExtension) <$> listDirectory dir
        validities <- forM files $ \file -> (file,) . valid () . (read :: String -> MLM) <$> readFile file
        return $ filter snd validities `shouldSatisfy` null
  describe "valid" $
    let dir = "should_narrowly_pass/" in
    it ("correctly judges content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir ++) <$> listDirectory dir
        validities <- forM files $ \file -> (file,) . valid () <$> fromXModeler file
        return $ filter (not . snd) validities `shouldSatisfy` null
  describe "valid" $
    let dir = "XModeler-strange/" in
    it ("correctly judges content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir ++) <$> listDirectory dir
        validities <- forM files $ \file -> (file,) . valid () <$> fromXModeler file
        return $ filter (not . snd) validities `shouldSatisfy` null

withIsolatedObjects :: [Int]
withIsolatedObjects = [72,220,282,562,582,602,676,734,748,872,882,890,984,1130,1172,1244,1312,1340,1374,1546,1594,1633,1685,1711,1820,1884,1984,2063,2264]
