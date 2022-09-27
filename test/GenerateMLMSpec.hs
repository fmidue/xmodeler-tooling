module GenerateMLMSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (forAll, oneof)
import Config (reasonableConfigs, smallConfigs)
import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.Types (valid, MLM(..), Class(..), Association(..), Multiplicity(..), Name(..), Link(..))
import Modelling.MLM.Config (Config(..))
import Data.Maybe (isJust, mapMaybe)

spec :: Spec
spec = do
        describe "generateMLM" $
          it "creates valid MLMs" $
            forAll (oneof [reasonableConfigs, smallConfigs]) $ \config ->
            forAll (generateMLM config) $
            valid ()
        describe "generateMLM" $
          it "strangely never puts attributes at abstract classes" $
            forAll (oneof [reasonableConfigs, smallConfigs]) $ \config ->
            forAll (generateMLM config) $ \MLM{classes} ->
              all (\Class{isAbstract, attributes} -> not isAbstract || null attributes) classes
        describe "generateMLM" $
          it "respects certain configuration parameters" $
            forAll (oneof [reasonableConfigs, smallConfigs]) $ \config@Config{maxClassLevel, numberOfClasses, numberOfAssociations} ->
            forAll (generateMLM config) $ \MLM{classes, associations} ->
              maximum (map (\Class{level} -> level) classes) == maxClassLevel &&
              length classes == numberOfClasses &&
              length associations == numberOfAssociations
        describe "generateMLM" $
          it "respects numberOfAttributesPerConcretization" $
            forAll (oneof [reasonableConfigs, smallConfigs]) $ \config@Config{numberOfAttributesPerConcretization} ->
            forAll (generateMLM config) $ \MLM{classes} ->
              let
                (average,0) =
                  divMod
                    (sum (map (\Class{attributes} -> length attributes) classes))
                    (length (filter (\Class{classifier} -> isJust classifier) classes))
              in
                average `shouldSatisfy` (== numberOfAttributesPerConcretization)
        describe "generateMLM" $
          it "respects tendencyAbstractClass" $
            forAll reasonableConfigs $ \config@Config{tendencyAbstractClass} ->
            forAll (generateMLM config) $ \MLM{classes} ->
              let
                allClassifiers = mapMaybe #classifier classes :: [Name]
                probability =
                  fromIntegral (length (filter (\Class{isAbstract} -> isAbstract) classes))
                  / fromIntegral (length (filter (\Class{level, name} -> level > 0 && name `notElem` allClassifiers) classes)) :: Float
              in
                probability `shouldSatisfy` within 0.5 tendencyAbstractClass
        describe "generateMLM" $
          it "respects tendencyConcretize" $
            forAll reasonableConfigs $ \config@Config{tendencyToConcretize} ->
            forAll (generateMLM config) $ \MLM{classes} ->
              let
                probability =
                  fromIntegral (length (filter (\Class{classifier} -> isJust classifier) classes))
                  / fromIntegral (length classes)
              in
                probability `shouldSatisfy` within 0.5 tendencyToConcretize
        describe "generateMLM" $
          it "respects multiplicitySpecAssociations" $
            forAll reasonableConfigs $ \config@Config{multiplicitySpecAssociations} ->
            forAll (generateMLM config) $ \MLM{associations} ->
              let
                multiplicities = concatMap (\Association{multSource, multTarget} -> [multSource, multTarget]) associations
                probability =
                  fromIntegral (length (filter (\(Multiplicity (_, upper)) -> isJust upper) multiplicities))
                  / fromIntegral (length multiplicities)
              in
                probability `shouldSatisfy` within 0.25 (fst multiplicitySpecAssociations)
        describe "generateMLM" $
          it "respects chanceVisibleAssociation" $
            forAll reasonableConfigs $ \config@Config{chanceVisibleAssociation} ->
            forAll (generateMLM config) $ \MLM{associations} ->
              let
                visibilities = concatMap (\Association{visibleSource, visibleTarget} -> [visibleSource, visibleTarget]) associations
                probability =
                  fromIntegral (length (filter id visibilities))
                  / fromIntegral (length visibilities)
              in
                probability `shouldSatisfy` within 0.25 chanceVisibleAssociation
        describe "generateMLM" $
          it "can generate a valid MLM with the maximum number of links (instances of associations) possible" $
            forAll (fmap (\config -> config{portionOfPossibleLinksToKeep = 1.0}) smallConfigs) $ \config ->
              forAll (generateMLM config) $ \mlm@MLM{classes, links, associations} -> let
                  cartesianProductOfClasses = cartesianProduct classes classes :: [(Class, Class)]
                  associationsNames = map #name associations :: [Name]
                  linksInfo = cartesianProduct associationsNames cartesianProductOfClasses :: [(Name, (Class, Class))]
                  newLinks = map (\(linkName, (class1, class2)) -> Link linkName (#name class1) (#name class2)) linksInfo :: [Link]
                  possibleLinksToAdd = filter (\x ->
                                                  let mlmWithTheNewLink = mlm{links = x : links} :: MLM
                                                  in valid () mlmWithTheNewLink
                                              ) newLinks :: [Link]
                  in
                    possibleLinksToAdd `shouldSatisfy` null

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

within :: Float -> Float -> Float -> Bool
within m p q = abs (p - q) < m
