{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
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

-- Type
instance XModelerable () Type where
  get () t = let
    cor = "XCore::" ++ show t
    aux = "Auxiliary::" ++ show t
    in
      case t of
        Boolean -> cor
        Integer -> cor
        Float -> cor
        String -> cor
        Element -> cor
        _ -> aux

-- Object
instance XModelerable String Object where
  get projectName (objectName, (x, y)) =
    [i|        <Object hidden="false" ref="Root::#{projectName}::#{objectName}" x="#{x}" y="#{y}"/>\n|]

-- Class (meta and instance) but without its content
instance XModelerable (Maybe Class, String) Class where
  get (maybeIsOf, projectName) (Class {isAbstract, cLevel, cName = name}) =
    case maybeIsOf of
      Nothing ->
        [i|    <addMetaClass abstract="#{isAbstract}" level="#{cLevel}" name="#{name}" package="Root::#{projectName}" parents=""/>\n|]
      Just isOf ->
        [i|    <addInstance abstract="#{isAbstract}" name="#{name}" of="Root::#{projectName}::#{cName isOf}" package="Root::#{projectName}" parents=""/>\n|]

-- Multiplicity
instance XModelerable () Multiplicity where
  get () (Multiplicity {lower, upper}) =
    [i|Seq{#{lower},#{upper},#{show (upper /= -1)},false}|]

-- Attributes
instance XModelerable (String, String) Attribute where
  get (className, projectName) (Attribute {multiplicity, tLevel, tName, tType}) =
    [i|    <addAttribute class="Root::#{projectName}::#{className}" level="#{tLevel}" multiplicity="#{get () multiplicity}" name="#{tName}" package="Root::#{projectName}" type="Root::#{get () tType}"/>\n|]

-- Operation
instance XModelerable (String, String) Operation where
  get (projectName, className) (Operation {body, oLevel, isMonitored, oName, oType}) =
    [i|    <addOperation body="#{body}" class="Root::${projectName}::#{className}" level="#{oLevel}" monitored="#{isMonitored}" name="#{oName}" package="Root::#{projectName}" paramNames="" paramTypes="" type="Root::#{get () oType}"/>\n|]

-- Parent
instance XModelerable (String, ()) Class where
  get (projectName, ()) (Class {cName}) =
    [i|Root::#{projectName}::#{cName},|]

-- Parents
instance XModelerable (String, String) [Class] where
  get (className, projectName) parents =
    [i|    <changeParent class="Root::#{projectName}::#{className}" new="#{checkParentsExistFirst}" old="" package="Root::#{projectName}"/>\n|]
    where checkParentsExistFirst = if null parents then "" else init (concatMap (get (projectName, ())) parents)

-- Slot
instance XModelerable (String, String) Slot where
  get (projectName, className) (Slot {attribute, value}) =
    [i|    <changeSlotValue class="Root::#{projectName}::#{className}" package="Root::#{projectName}" slotName="#{tName attribute}" valueToBeParsed="#{value}"/>\n|]

-- Class (meta or instance) with its content
instance XModelerable String Class where
  get projectName class'@(Class {cName, cIsOf, attributes, operations, parents, slots}) =
    get (cIsOf, projectName) class' ++
    concatMap (get (cName, projectName)) attributes ++
    concatMap (get (cName, projectName)) operations ++
    get (cName, projectName) parents ++
    concatMap (get (projectName, cName)) slots

-- Association
instance XModelerable String Association where
  get projectName (Association {sName, sSource, sTarget, lvlSource, lvlTarget, multSourceToTarget, multTargetToSource, sourceVisibleFromTarget, targetVisibleFromSource}) =
    [i|    <addAssociation accessSourceFromTargetName="#{sName}_#{from}" accessTargetFromSourceName="#{sName}_#{to}" classSource="Root::#{projectName}::#{from}" classTarget="Root::#{projectName}::#{to}" fwName="#{sName}" instLevelSource="#{lvlSource}" instLevelTarget="#{lvlTarget}" isSymmetric="false" isTransitive="false" multSourceToTarget="#{get () multSourceToTarget}" multTargetToSource="#{get () multTargetToSource}" package="Root::#{projectName}" reverseName="-1" sourceVisibleFromTarget="#{sourceVisibleFromTarget}" targetVisibleFromSource="#{targetVisibleFromSource}"/>\n|]
   where
     from = cName sSource
     to = cName sTarget

-- Link
instance XModelerable String Link where
  get projectName (Link {lSource, lTarget, lIsOf}) =
    [i|    <addLink classSource="Root::#{projectName}::#{cName lSource}" classTarget="Root::#{projectName}::#{cName lTarget}" name="#{sName lIsOf}" package="Root::#{projectName}"/>\n|]

-- MLM
instance XModelerable ([Object], Double, Int) MLM where
  get (objects', scaleFactor, extraOffset)
      (MLM {projectName, classes, associations, links}) =
    let
      xs = map (fst . snd) objects'
      ys = map (snd . snd) objects'
      getRange list = fromIntegral $ maximum list - minimum list
      xx = log $ max (getRange xs) (getRange ys) * scaleFactor / 1000 :: Double
      txTy = round $ 50 * xx :: Int

      objects'' = map (second (first (+ extraOffset))) objects' :: [Object]
      objectsXML = concatMap (get projectName) objects'' :: String
      classesXML = concatMap (get projectName) classes :: String
      associationsXML = concatMap (get projectName) associations :: String
      linksXML = concatMap (get projectName) links:: String
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