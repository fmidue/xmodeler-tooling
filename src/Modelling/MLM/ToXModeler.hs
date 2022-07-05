{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Modelling.MLM.ToXModeler (toXModeler) where

import Data.Char (ord)
import Data.Map.Strict (toList)
import Data.String.Interpolate (i)
import Data.GraphViz (GraphvizCommand)
import Diagrams.Points (Point (P))
import Diagrams.TwoD.Types (V2 (V2))
import Diagrams.TwoD.GraphViz (layoutGraph, mkGraph, getGraph)
import Data.Maybe (isNothing, isJust)
import Modelling.MLM.Types (
  MLM (..),
  Link (..),
  Association (..),
  Slot (..),
  Class (..),
  Operation (..),
  Attribute (..),
  Multiplicity (..),
  Value (..),
  Type (..),
  relativeToEur,
  currencySymbol
  )

data Object = Object {
  objName :: String,
  objX :: Int,
  objY :: Int
} deriving Show

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

-- Value
instance XModelerable () Value where
  get () value =
    case value of
      B b -> show b
      I i' -> show i'
      F f -> show f
      S s -> [i|#{map ord s}.asString()|]
      M amount currency ->
        [i|Auxiliary::MonetaryValue(#{amount}, Auxiliary::Currency(&quot;#{currency}&quot;, &quot;#{currency}&quot;, 1.0))|]
      D year month day ->
        [i|Auxiliary::Date::createDate(#{year}, #{month}, #{day})|]
      C c ->
        [i|Auxiliary::Currency(&quot;#{currencySymbol c}&quot;, &quot;#{c}&quot;, #{relativeToEur c})|]
      _ -> "null"

-- Object
instance XModelerable String Object where
  get projectName (Object {objName, objX, objY}) =
    [i|        <Object hidden="false" ref="Root::#{projectName}::#{objName}" x="#{objX}" y="#{objY}"/>\n|]

-- Class (MetaClass) but without its content
instance XModelerable (String, ()) Class where
  get (projectName, ()) (Class {isAbstract, cLevel, cName = name}) =
    [i|    <addMetaClass abstract="#{isAbstract}" level="#{cLevel}" name="#{name}" package="Root::#{projectName}" parents=""/>\n|]

-- Class (Instance) but without its content
instance XModelerable (String, (), ()) Class where
  get projectName (Class {isAbstract, cName = name, cIsOf}) =
    case cIsOf of
      Nothing -> error "That should not happen! A class is an instance and a metaclass at the same time!"
      Just isOf ->
        [i|    <addInstance abstract="#{isAbstract}" name="#{name}" of="Root::#{projectName}::#{cName isOf}" package="Root::#{projectName}" parents=""/>\n|]

-- Multiplicity
instance XModelerable () Multiplicity where
  get () (Multiplicity {lower, upper}) =
    [i|Seq{#{lower},#{upper},#{show (upper /= -1)},false}|]

-- Attributes
instance XModelerable (String, String) Attribute where
  get (projectName, className) (Attribute {multiplicity, tLevel, tName, tType}) =
    [i|    <addAttribute class="Root::#{projectName}::#{className}" level="#{tLevel}" multiplicity="#{get () multiplicity}" name="#{tName}" package="Root::#{projectName}" type="Root::#{get () tType}"/>\n|]

-- Operation
instance XModelerable (String, String) Operation where
  get (projectName, className) (Operation {body, oLevel, isMonitored, oName, oType}) =
    [i|    <addOperation body="#{body}" class="Root::${projectName}::#{className}" level="#{oLevel}" monitored="#{isMonitored}" name="#{oName}" package="Root::#{projectName}" paramNames="" paramTypes="" type="Root::#{get () oType}"/>\n|]

-- Parent
instance XModelerable String Class where
  get projectName (Class {cName}) =
    [i|Root::#{projectName}::#{cName},|]

-- Parents
instance XModelerable (String, String) [Class] where
  get _ [] = ""
  get (projectName, className) parents =
    [i|    <changeParent class="Root::#{projectName}::#{className}" new="#{doParents}" old="" package="Root::#{projectName}"/>\n|]
    where doParents = init (concatMap (get projectName) parents)

-- Slot
instance XModelerable (String, String) Slot where
  get (projectName, className) (Slot {attribute, value}) =
    [i|    <changeSlotValue class="Root::#{projectName}::#{className}" package="Root::#{projectName}" slotName="#{tName attribute}" valueToBeParsed="#{get () value}"/>\n|]

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
  get (objects, xx, txTy)
      (MLM {projectName, classes, associations, links}) =
    let
      -- I can avoid some of the following transformations if I am allowed to include the
      -- containing class (the "Mother") of a component (operation, slot, attribute) as an
      -- attribute (field) in the type of that component and simplify the instances XModelerable
      -- by letting them just get the containing class from that field within the inputted
      -- attribute, operation, or slot
      pairWithMother f x  = map (cName x,) (f x)

      allObjects            = objects
      allMetaclasses        = filter (isNothing . cIsOf) classes :: [Class]
      allInstances          = filter (isJust . cIsOf) classes :: [Class]
      allInheritances0      = filter (not . null . parents) classes :: [Class]
      allInheritances       = map (\x -> (cName x, parents x)) allInheritances0 :: [(String, [Class])]
      allAttributes         = concatMap (pairWithMother attributes) classes :: [(String, Attribute)]
      allOperations         = concatMap (pairWithMother operations) classes :: [(String, Operation)]
      allChangeSlotValues   = concatMap (pairWithMother slots) classes      :: [(String, Slot)]
      allAssociations       = associations
      allLinks              = links

      allObjectsXML          = concatMap (get projectName) allObjects :: String
      allMetaclassesXML      = concatMap (get (projectName, ())) allMetaclasses :: String
      allInstancesXML        = concatMap (get (projectName, (), ())) allInstances :: String
      allInheritancesXML     = concatMap (\(className, x) -> get (projectName, className) x) allInheritances :: String
      allAttributesXML       = concatMap (\(className, x) -> get (projectName, className) x) allAttributes :: String
      allOperationsXML       = concatMap (\(className, x) -> get (projectName, className) x) allOperations :: String
      allChangeSlotValuesXML = concatMap (\(className, x) -> get (projectName, className) x) allChangeSlotValues :: String
      allAssociationsXML     = concatMap (get projectName) allAssociations :: String
      allLinksXML            = concatMap (get projectName) allLinks :: String

      rmLnBrk :: String -> String
      rmLnBrk "" = ""
      rmLnBrk x = init x
    in
      [i|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
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
#{rmLnBrk allObjectsXML}
      </Objects>
      <Edges/>
      <Labels/>
      <Preferences/>
      <View name="Main View" tx="#{txTy}" ty="#{txTy}" xx="#{xx}"/>
    </Diagram>
  </Diagrams>
  <Logs>
#{rmLnBrk allMetaclassesXML}
#{rmLnBrk allInstancesXML}
#{rmLnBrk allInheritancesXML}
#{rmLnBrk allAttributesXML}
#{rmLnBrk allOperationsXML}
#{rmLnBrk allChangeSlotValuesXML}
#{rmLnBrk allAssociationsXML}
#{rmLnBrk allLinksXML}
  </Logs>
</XModeler>|]

toXModeler :: (GraphvizCommand, Double -> Double, Double, Int ) -> MLM -> IO String
toXModeler    (layoutCommand, spaceOut, scaleFactor, extraOffset)
              mlm@(MLM {classes, associations, links}) = let
    vertices = map cName classes :: [String]
    extractEdge from to x = (cName (from x), cName (to x), ()) :: (String, String, ())
    edges = map (extractEdge sSource sTarget) associations ++
            map (extractEdge lSource lTarget) links ++
            concatMap (\child -> map (\parent -> (cName child, cName parent, ())) (parents child)) classes
             :: [(String, String, ())]
    adjust :: Double -> Int
    adjust = round . spaceOut
    getRange [] = 0
    getRange list = fromIntegral $ maximum list - minimum list
    extractObject (vertex, P (V2 x y)) = Object vertex (extraOffset + adjust x) (adjust y)
  in do
    g <- layoutGraph layoutCommand $ mkGraph vertices edges
    let objects = map extractObject $ toList $ fst $ getGraph g :: [Object]
    let xs = map objX objects
    putStrLn "xs="
    print xs
    let ys = map objY objects
    putStrLn "ys="
    print ys
    let xx = log $ max (getRange xs) (getRange ys) * scaleFactor / 1000 :: Double
    putStrLn "xx="
    print xx
    let txTy = round $ 50 * xx :: Int
    putStrLn "txty="
    print txTy
    return $ get (objects, xx, txTy) mlm