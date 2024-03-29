module ValidatableSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (ioProperty)

import Data.List ((\\))
import Data.List.Extra (nubOrd)
import Control.Monad (filterM, forM_, forM, when, unless)
import System.Directory (listDirectory)
import System.Directory.Internal (getFileMetadata, fileTypeFromMetadata, fileTypeIsDirectory)
import System.FilePath.Posix (takeExtension, (</>))

import Modelling.MLM.FromXModeler (fromXModeler)
import Modelling.MLM.Types
  ( MLM
  , LeniencyConsideringConcretization(..)
  , LeniencyConsideringSlotFilling(..)
  , LeniencyConsideringLowerMultiplicities(..)
  , beVeryStrict
  )
import Modelling.MLM.Validate (valid)

import Text.Pretty.Simple (pPrint)

import Control.Exception (evaluate)
import Control.DeepSeq (force)

spec :: Spec
spec = do
  -- total number of files is 2292
  forM_ ([1..2292] \\ withIsolatedObjects) $ \i ->
      let file = "examples" </> "UML" </> "testing_" ++ show i ++ ".xml" in
      describe "valid" $
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- evaluate . force =<< fromXModeler file
            return $ input `shouldSatisfy` valid beVeryStrict
  forM_ withIsolatedObjects $ \i ->
      let file = "examples" </> "UML" </> "testing_" ++ show i ++ ".xml" in
      describe "valid" $
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- evaluate . force =<< fromXModeler file
            return $ input `shouldSatisfy`
              valid
              ( BeLenientAboutConcretization
              , BeStrictAboutSlotFilling
              , BeStrictAboutLowerMultiplicities)
  forM_ withIsolatedObjects $ \i ->
      let file = "examples" </> "UML" </> "testing_" ++ show i ++ ".xml" in
      describe "invalid" $
        it ("correctly judges " ++ file) $
          ioProperty $ do
            input <- evaluate . force =<< fromXModeler file
            return $ input `shouldSatisfy` (not . valid beVeryStrict)
  describe "invalid" $
   let dir = "examples" </> "should_fail" in do
    it ("correctly judges .xml content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir </>) . filter ((".xml" ==) . takeExtension) <$> listDirectory dir
        validities <- forM files $ \file -> do
          mlm <- evaluate . force =<< fromXModeler file
          let validity = (file,) . valid beVeryStrict $ mlm
          when (snd validity) $ putStrLn file >> pPrint mlm
          return validity
        return $ filter snd validities `shouldSatisfy` null
    it ("correctly judges .hs content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir </>) . filter ((".hs" ==) . takeExtension) <$> listDirectory dir
        validities <- forM files $ \file -> (file,) .
                                            valid
                                            ( BeLenientAboutConcretization
                                            , BeStrictAboutSlotFilling
                                            , BeStrictAboutLowerMultiplicities)
                                            . (read :: String -> MLM) <$> readFile file
        return $ filter snd validities `shouldSatisfy` null
  describe "valid" $
    let dir = "examples" </> "should_narrowly_pass" in
    it ("correctly judges content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir </>) <$> listDirectory dir
        validities <- forM files $ \file -> do
          mlm <- evaluate . force =<< fromXModeler file
          let validity = (file,) . valid beVeryStrict $ mlm
          unless (snd validity) $ putStrLn file >> pPrint mlm
          return validity
        return $ filter (not . snd) validities `shouldSatisfy` null
  describe "valid" $
    let dir = "examples" </> "XModeler-strange" in
    it ("correctly judges content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir </>) <$> listDirectory dir
        validities <- forM files $ \file -> (file,) . valid beVeryStrict <$> (evaluate . force =<< fromXModeler file)
        return $ filter (not . snd) validities `shouldSatisfy` null
  describe "valid" $
    let dir = "examples" </> "should_pass" in
    it ("correctly judges content of " ++ dir ++ " directory") $
      ioProperty $ do
        files <- map (dir </>) <$> listDirectory dir
        validities <- forM files $ \file -> (file,) . valid beVeryStrict . (read :: String -> MLM) <$> readFile file
        return $ filter (not . snd) validities `shouldSatisfy` null
  describe "valid" $
    let dir = "examples" </> "should_have_same_outcome" in
    it ("makes consistent judgements in each subdirectory of " ++ dir ++ " directory") $
      ioProperty $ do
        subdirs <- filterM (fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata)
                    . map (dir </>) =<< listDirectory dir
        results <- forM subdirs $ \subdir ->
          listDirectory subdir >>= mapM ((\file -> (file,) . valid beVeryStrict <$> (evaluate . force =<< fromXModeler file)) . (subdir </>))
        return $ filter ((1<) . length . nubOrd . map snd) results `shouldSatisfy` null

withIsolatedObjects :: [Int]
withIsolatedObjects = [72,220,282,562,582,602,676,734,748,872,882,890,984,1130,1172,1244,1312,1340,1374,1546,1594,1633,1685,1711,1820,1884,1984,2063,2264]
