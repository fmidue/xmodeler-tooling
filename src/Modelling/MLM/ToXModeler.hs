{-# LANGUAGE QuasiQuotes #-}

module Modelling.MLM.ToXModeler (toXModeler) where

import Data.Char (ord)
import Data.Map.Strict (toList)
import Data.String.Interpolate (i)
import Data.GraphViz (GraphvizCommand)
import Data.Maybe (isJust, isNothing)
import Diagrams.Points (Point (P))
import Diagrams.TwoD.Types (V2 (V2))
import Diagrams.TwoD.GraphViz (layoutGraph, mkGraph, getGraph)
import Modelling.MLM.Types (
  MLM (..),
  Link (..),
  Association (..),
  Slot (..),
  Class (..),
  Operation (..),
  Attribute (..),
  Multiplicity (..),
  Type (..),
  Name (..),
  OperationBody (..),
  getTypeName,
  isUnassigned,
  relativeToEur,
  currencySymbol
  )

newtype AsUnclassified = AsUnclassified Class
newtype AsInstance = AsInstance Class
newtype AsChild = AsChild Class

data Object = Object {
  name :: Name,
  x :: Int,
  y :: Int
} deriving Show

class XModelerable context a where
  get :: context -> a -> String

-- Name : no need, because `get () Name` would be just `show Name`

-- Type
instance XModelerable () Type where
  get () t = let
    cor = "XCore::" ++ getTypeName t
    aux = "Auxiliary::" ++ getTypeName t
    in
    if isUnassigned t then
      case t of
        Boolean _ -> cor
        Integer _ -> cor
        Float _ -> cor
        String _ -> cor
        Element _ -> cor
        _ -> aux
    else
      case t of
        Boolean (Just b) -> show b
        Integer (Just i') -> show i'
        Float (Just f) -> show f
        String (Just s)-> [i|#{map ord s}.asString()|]
        MonetaryValue (Just (amount, currency)) ->
          [i|Auxiliary::MonetaryValue(#{amount}, Auxiliary::Currency(&quot;#{currency}&quot;, &quot;#{currency}&quot;, 1.0))|]
        Date (Just (year, month, day)) ->
          [i|Auxiliary::Date::createDate(#{year}, #{month}, #{day})|]
        Currency (Just c) ->
          [i|Auxiliary::Currency(&quot;#{currencySymbol c}&quot;, &quot;#{c}&quot;, #{relativeToEur c})|]
        _ -> "null"

-- Object
instance XModelerable Name Object where
  get projectName (Object {name, x, y}) =
    [i|        <Object hidden="false" ref="Root::#{projectName}::#{name}" x="#{x}" y="#{y}"/>\n|]

-- Class
instance XModelerable Name AsUnclassified where
  get projectName (AsUnclassified (Class {isAbstract, level, name, classifier = Nothing})) =
    [i|    <addMetaClass abstract="#{isAbstract}" level="#{level}" name="#{name}" package="Root::#{projectName}" parents=""/>\n|]
  get _ _ = error "ERROR! This class is supposed to have no classifier, but it seems to have one!"

--  Class : Instance
instance XModelerable Name AsInstance where
  get projectName (AsInstance (Class {isAbstract, name, classifier = Just classifierName})) =
    [i|    <addInstance abstract="#{isAbstract}" name="#{name}" of="Root::#{projectName}::#{classifierName}" package="Root::#{projectName}" parents=""/>\n|]
  get _ _ = error "ERROR! This class is supposed to have a classifier, but it seems to have none!"

-- Class : ChangeParents (Child)
instance XModelerable Name AsChild where
  get projectName (AsChild (Class {name, parents})) =
    [i|    <changeParent class="Root::#{projectName}::#{name}" new="#{doParents}" old="" package="Root::#{projectName}"/>\n|]
      where doParents = init (concatMap (\p -> [i|Root::#{projectName}::#{p},|]) parents)

-- Multiplicity
instance XModelerable () Multiplicity where
  get () (Multiplicity (lower, upper)) =
    [i|Seq{#{lower},#{upper},#{show (upper /= -1)},false}|]

-- Attribute
instance XModelerable (Name, Name) Attribute where
  get (projectName, className) (Attribute {level, name, dataType, multiplicity}) =
    [i|    <addAttribute class="Root::#{projectName}::#{className}" level="#{level}" multiplicity="#{get () multiplicity}" name="#{name}" package="Root::#{projectName}" type="Root::#{get () dataType}"/>\n|]

-- OperationBody
instance XModelerable Name OperationBody where
  get _ _ = "STILL UNDEFINED OPERATION BODY"

-- Operation
instance XModelerable (Name, Name) Operation where
  get (projectName, attributeClass) (Operation {body, level, isMonitored, name, dataType}) =
    [i|    <addOperation body="#{get projectName body}" class="Root::${projectName}::#{attributeClass}" level="#{level}" monitored="#{isMonitored}" name="#{name}" package="Root::#{projectName}" paramNames="" paramTypes="" type="Root::#{get () dataType}"/>\n|]

-- Slot
instance XModelerable (Name, Name) Slot where
  get (projectName, slotClass) (Slot {attribute, value}) =
    [i|    <changeSlotValue class="Root::#{projectName}::#{slotClass}" package="Root::#{projectName}" slotName="#{attribute}" valueToBeParsed="#{get () value}"/>\n|]

-- Association
instance XModelerable Name Association where
  get projectName (Association {name, source, target, lvlSource, lvlTarget, multSourceToTarget, multTargetToSource, sourceVisibleFromTarget, targetVisibleFromSource}) =
    [i|    <addAssociation accessSourceFromTargetName="#{name}_#{from}" accessTargetFromSourceName="#{name}_#{to}" classSource="Root::#{projectName}::#{from}" classTarget="Root::#{projectName}::#{to}" fwName="#{name}" instLevelSource="#{lvlSource}" instLevelTarget="#{lvlTarget}" isSymmetric="false" isTransitive="false" multSourceToTarget="#{get () multSourceToTarget}" multTargetToSource="#{get () multTargetToSource}" package="Root::#{projectName}" reverseName="-1" sourceVisibleFromTarget="#{sourceVisibleFromTarget}" targetVisibleFromSource="#{targetVisibleFromSource}"/>\n|]
   where
     from = show source
     to = show target

-- Link
instance XModelerable Name Link where
  get projectName (Link {source, target, association}) =
    [i|    <addLink classSource="Root::#{projectName}::#{source}" classTarget="Root::#{projectName}::#{target}" name="#{association}" package="Root::#{projectName}"/>\n|]

-- MLM
instance XModelerable ([Object], Double, Int) MLM where
  get (mlmObjects, xx, txTy) (MLM {name = projectName, classes, associations, links}) =
    let
      allTagsObject =
        concatMap (get projectName) mlmObjects
      allTagsMetaClass =
        concatMap (get projectName . AsUnclassified) $
          filter (isNothing . #classifier) classes
      allTagsInstance =
        concatMap (get projectName . AsInstance) $
          filter (isJust . #classifier) classes
      allTagsChangeParents =
        concatMap (get projectName . AsChild) $
          filter (not . null . #parents) classes
      allTagsAttribute =
        concatMap
          (\x -> concatMap (get (projectName, #name x)) (#attributes x))
          classes
      allTagsOperation =
        concatMap
          (\x -> concatMap (get (projectName, #name x)) (#operations x))
          classes
      allTagsSlot =
        concatMap
          (\x -> concatMap (get (projectName, #name x)) (#slots x))
          classes
      allTagsAssociation = concatMap (get projectName) associations
      allTagsLink = concatMap (get projectName ) links
    in
      [i|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<XModeler>
  <Version>2</Version>
  <Categories/>
  <Projects>
    <Project name="Root::#{show projectName}"/>
  </Projects>
  <Diagrams>
    <Diagram label="#{show projectName}_diagram" package_path="Root::#{show projectName}" showConstraintReports="false" showConstraints="false" showDerivedAttributes="false" showDerivedOperations="false" showGettersAndSetters="false" showMetaClassName="false" showOperationValues="true" showOperations="true" showSlots="true">
      <Categories/>
      <Owners/>
      <Objects>
#{allTagsObject}
      </Objects>
      <Edges/>
      <Labels/>
      <Preferences/>
      <View name="Main View" tx="#{txTy}" ty="#{txTy}" xx="#{xx}"/>
    </Diagram>
  </Diagrams>
  <Logs>
#{allTagsMetaClass}
#{allTagsInstance}
#{allTagsChangeParents}
#{allTagsAttribute}
#{allTagsOperation}
#{allTagsSlot}
#{allTagsAssociation}
#{allTagsLink}
  </Logs>
</XModeler>|]

toXModeler :: (GraphvizCommand, Double -> Double, Double, Int ) -> MLM -> IO String
toXModeler    (layoutCommand, spaceOut, scaleFactor, extraOffset)
              mlm@(MLM {classes, associations, links}) = let
    vertices = map (show . #name) classes :: [String]
    extractEdge from to x =
      (show (from x), show (to x), ()) :: (String, String, ())
    edges =
      map (extractEdge #source #target) associations ++
      map (extractEdge #source #target) links ++
      concatMap
        (\child -> map
          (\parent -> (show (#name child), show parent, ()))
          (#parents child))
        classes :: [(String, String, ())]

    adjust :: Double -> Int
    adjust = round . spaceOut

    extractObject (vertex, P (V2 x y)) = Object (Name vertex) (extraOffset + adjust x) (adjust y)
  in do
    g <- layoutGraph layoutCommand $ mkGraph vertices edges
    let objects = map extractObject $ toList $ fst $ getGraph g :: [Object]
    -- let xs = map #x objects
    -- let ys = map #y objects
    let xx = 1 / sqrt ( sqrt ( sqrt ( fromIntegral (length classes)))) * scaleFactor :: Double
    let xxTruncated = round (xx * 100 :: Double) :: Int
    putStrLn $ "view scale = " ++ show xxTruncated ++ "%"
    let txTy = extraOffset * round xx :: Int
    return $ get (objects, xx, txTy) mlm