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

class XModelerable c a where
  get :: c -> a -> String

instance XModelerable () Type where
  get () x = let
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

instance XModelerable String Object where
  get projectName (objectName, (x, y)) =
    [i|        <Object hidden="false" ref="Root::#{projectName}::#{objectName}" x="#{x}" y="#{y}"/>\n|]

instance XModelerable (Maybe Class, String) Class where
  get (Nothing, projectName) c =
    [i|    <addMetaClass abstract="#{isAbstract c}" level="#{cLevel c}" name="#{cName c}" package="Root::#{projectName}" parents=""/>\n|]
  get (Just isOf, projectName) c =
    [i|    <addInstance abstract="#{isAbstract c}" name="#{cName c}" of="Root::#{projectName}::#{cName isOf}" package="Root::#{projectName}" parents=""/>\n|]

instance XModelerable () Multiplicity where
  get () (Multiplicity lowerBound upperBound) =
    [i|Seq{#{lowerBound},#{upperBound},#{show (upperBound /= -1)},false}|]

instance XModelerable (String, String) Attribute where
  get (className, projectName) t =
    [i|    <addAttribute class="Root::#{projectName}::#{className}" level="#{tLevel t}" multiplicity="#{get () (multiplicity t)}" name="#{tName t}" package="Root::#{projectName}" type="Root::#{get () (tType t)}"/>\n|]

instance XModelerable (String, String) Operation where
  get _ _ = "TODO OPERATIONS\n"

instance XModelerable (String, ()) Class where
  get (projectName, ()) parent =
    [i|Root::#{projectName}::#{cName parent},|]

instance XModelerable (String, String) [Class] where
  get (className, projectName) prnts =
    [i|    <changeParent class="Root::#{projectName}::#{className}" new="#{checkParentsExistFirst}" old="" package="Root::#{projectName}"/>\n|]
    where checkParentsExistFirst = if null prnts then "" else init (concatMap (get (projectName, ())) prnts)

instance XModelerable () Slot where
  get _ _ = "TODO SLOTS (ATTRIBUTES AND OPERATIONS)\n"

instance XModelerable String Class where
  get projectName c =
    get (cIsOf c, projectName) c ++
    concatMap (get (cName c, projectName)) (attributes c) ++
    concatMap (get (cName c, projectName)) (operations c) ++
    get (cName c, projectName) (parents c) ++
    concatMap (get ()) (slots c) -- slots

instance XModelerable String Association where
  get _ _ = "TODO GET ASSOCIATION"

instance XModelerable String Link where
  get _ _ = "TODO GET LINK"

instance XModelerable ([Object], Double, Int)  MLM where
  get (objects', scaleFactor, extraOffset)
      (MLM projectName mlmClasses mlmAssociations mlmLinks) =
    let
      xs = map (fst . snd) objects'
      ys = map (snd . snd) objects'
      getRange list = fromIntegral $ maximum list - minimum list
      xx = log $ max (getRange xs) (getRange ys) * scaleFactor / 1000 :: Double
      txTy = round $ 50 * xx :: Int

      objects'' = map (second (first (+ extraOffset))) objects' :: [Object]
      objectsXML = concatMap (get projectName) objects'' :: String
      classesXML = concatMap (get projectName) mlmClasses :: String
      associationsXML = concatMap (get projectName) mlmAssociations :: String
      linksXML = concatMap (get projectName) mlmLinks :: String
    in [i|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<XModeler>
  <Version>2</Version>
  <Categories/>
  <Projects>
    <Project name="Root::#{projectName}"/>
  </Projects>
  <Diagrams>
    <Diagram label="#{projectName}_diagram" package_path="Root::#{projectName}" showConstraintReports="true" showConstraints="true" showDerivedAttributes="true" showDerivedOperations="true" showGettersAndSetters="true" showMetaClassName="false" showOperationValues="true" showOperations="true" showSlots="true">
      <Categories/>
      <Owners/>
      <Objects>
#{objectsXML}
      </Objects>
      <Edges/>
      <Labels/>
      <Preferences/>
      <View name="Main View" tx="#{txTy}" ty="#{txTy}" xx="#{xx}"/>
    </Diagram>
  </Diagrams>
  <Logs>
#{classesXML}
#{associationsXML}
#{linksXML}
  </Logs>
</XModeler>|]

toXModeler :: (GraphvizCommand, Double -> Double, Double, Int ) -> MLM -> IO String
toXModeler    (layoutCommand, spaceOut, scaleFactor, extraOffset)
              mlm@(MLM _ mlmClasses mlmAssociations mlmLinks) = let
    vertices = map cName mlmClasses :: [String]
    edges    = map (\x -> (cName (sSource x), cName (sTarget x), ())) mlmAssociations ++
               map (\x -> (cName (lSource x), cName (lTarget x), ())) mlmLinks :: [(String, String, ())]
    adjust :: Double -> Int
    adjust = round . spaceOut
  in do
    g <- layoutGraph layoutCommand $ mkGraph vertices edges
    let objects = map (\(vertex, P (V2 x y)) -> (vertex, (x, y))) $ toList $ fst $ getGraph g :: [(String, (Double, Double))]
    let objects' = map (second (bimap adjust adjust)) objects :: [Object]
    return $ get (objects', scaleFactor, extraOffset) mlm