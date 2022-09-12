module GenerateMLMSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (forAll)
import Config (reasonableConfigs, smallConfigs)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Modelling.MLM.GenerateMLM (generateMLM)
import Modelling.MLM.Types (valid, MLM(..), Class(..), Attribute(..), Association(..), Multiplicity(..), Name(..), Link(..))
import Modelling.MLM.Config (Config(..))
import Control.Monad (filterM)
import Data.Maybe (isJust)

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
              maximum (map (\Class{level} -> level) classes) == maxClassLevel &&
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
                  fromIntegral (length (filter (\Attribute{multiplicity = Multiplicity (_, upper)} -> isJust upper) theAttributes))
                  / fromIntegral (length theAttributes)
              in
                probability `shouldSatisfy` within 0.2 (fst multiplicitySpecAttributes)
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
                probability `shouldSatisfy` within 0.2 (fst multiplicitySpecAssociations)
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
                probability `shouldSatisfy` within 0.2 chanceVisibleAssociation
        describe "generateMLM" $
          modifyMaxSuccess (const 1) $
          it "can generate a valid MLM with the maximum number of links (instances of associations) possible" $
            forAll smallConfigs $ \config ->
              forAll (generateMLM config{portionOfPossibleLinksToKeep = 1.0}) $ \mlm@MLM{classes, links, associations} -> let
                  cartesianProductOfClasses = cartesianProduct classes classes :: [(Class, Class)]
                  associationsNames = map #name associations :: [Name]
                  linksInfo = cartesianProduct associationsNames cartesianProductOfClasses :: [(Name, (Class, Class))]
                  newLinks = map (\(linkName, (class1, class2)) -> Link linkName (#name class1) (#name class2)) linksInfo :: [Link]
                in do
                  possibleLinksTooAdd <- filterM (\x -> do
                      let mlmWithTheNewLink = mlm{links = x : links} :: MLM
                      if valid () mlmWithTheNewLink
                        then do
                          print x
                          return True
                        else
                          return False
                    ) newLinks :: IO [Link]
                  possibleLinksTooAdd `shouldSatisfy` null

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

within :: Float -> Float -> Float -> Bool
within m p q = abs (p - q) < m
