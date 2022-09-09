module GenerateMLMSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (forAll)

import Config (reasonableConfigs)

import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.Types (valid, MLM(..), Class(..), Attribute(..), Association(..), Multiplicity(..))
import Modelling.MLM.Config (Config(..))

import Data.Maybe (isJust, isNothing)

spec :: Spec
spec = do
        describe "generateMLM" $
          it "creates valid MLMs" $
            forAll reasonableConfigs $ \config ->
            forAll (generateMLM config) $
            valid ()
        describe "generateMLM" $
          it "respects certain configuration parameters" $
            forAll reasonableConfigs $ \config@Config{maxClassLevel, numberOfClasses, numberOfAssociations} ->
            forAll (generateMLM config) $ \MLM{classes, associations} ->
              maximum (map (\Class{level} -> level) classes) <= maxClassLevel &&
              length classes == numberOfClasses &&
              length associations == numberOfAssociations
        describe "generateMLM" $
          it "respects chanceAbstractClass" $
            forAll reasonableConfigs $ \config@Config{chanceAbstractClass} ->
            forAll (generateMLM config) $ \MLM{classes} ->
              let
                probability =
                  fromIntegral (length (filter (\Class{isAbstract} -> isAbstract) classes))
                  / fromIntegral (length classes) :: Float
              in
                probability `shouldSatisfy` within 0.4 chanceAbstractClass
        describe "generateMLM" $
          it "respects chanceToConcretize" $
            forAll reasonableConfigs $ \config@Config{chanceToConcretize} ->
            forAll (generateMLM config) $ \MLM{classes} ->
              let
                probability =
                  fromIntegral (length (filter (\Class{classifier} -> isJust classifier) classes))
                  / fromIntegral (length classes)
              in
                probability `shouldSatisfy` within 0.4 chanceToConcretize
        describe "generateMLM" $
          it "respects multiplicitySpecAttributes" $
            forAll reasonableConfigs $ \config@Config{multiplicitySpecAttributes} ->
            forAll (generateMLM config) $ \MLM{classes} ->
              let
                theAttributes = concatMap (\Class{attributes} -> attributes) classes
                probability =
                  fromIntegral (length (filter (\Attribute{multiplicity = Multiplicity (_, upper)} -> isNothing upper) theAttributes))
                  / fromIntegral (length theAttributes)
              in
                probability `shouldSatisfy` within 0.2 (fst multiplicitySpecAttributes)
        describe "generateMLM" $
          it "respects multiplicitySpecsAssociations" $
            forAll reasonableConfigs $ \config@Config{multiplicitySpecsAssociations} ->
            forAll (generateMLM config) $ \MLM{associations} ->
              let
                multiplicities = concatMap (\Association{multTargetToSource, multSourceToTarget} -> [multTargetToSource, multSourceToTarget]) associations
                probability =
                  fromIntegral (length (filter (\(Multiplicity (_, upper)) -> isNothing upper) multiplicities))
                  / fromIntegral (length multiplicities)
              in
                probability `shouldSatisfy` within 0.2 (fst multiplicitySpecsAssociations)
        describe "generateMLM" $
          it "respects chanceVisibleAssociation" $
            forAll reasonableConfigs $ \config@Config{chanceVisibleAssociation} ->
            forAll (generateMLM config) $ \MLM{associations} ->
              let
                visibilities = concatMap (\Association{sourceVisibleFromTarget, targetVisibleFromSource} -> [sourceVisibleFromTarget, targetVisibleFromSource]) associations
                probability =
                  fromIntegral (length (filter id visibilities))
                  / fromIntegral (length visibilities)
              in
                probability `shouldSatisfy` within 0.2 chanceVisibleAssociation

within :: Float -> Float -> Float -> Bool
within m p q = abs (p - q) < m
