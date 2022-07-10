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
  Name,
  getTypeName,
  isUnassigned,
  relativeToEur,
  currencySymbol
  )

data Tag = TagMetaClass | TagInstance | TagChangeOneParent

data Object = Object {
  objName :: Name,
  objX :: Int,
  objY :: Int
} deriving Show

class XModelerable c t a where
  get :: c -> t -> a -> String

-- Type
instance XModelerable () () Type where
  get () () t = let
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
instance XModelerable Name () Object where
  get mlmName' () (Object {objName, objX, objY}) =
    [i|        <Object hidden="false" ref="Root::#{mlmName'}::#{objName}" x="#{objX}" y="#{objY}"/>\n|]

-- Class : MetaClass or Instance or ChangeParent
instance XModelerable Name Tag Class where
  get mlmName' tag (Class {cIsAbstract, cLevel, cName = cName', cOf}) = case tag of
    TagMetaClass ->
      [i|    <addMetaClass abstract="#{cIsAbstract}" level="#{cLevel}" name="#{cName'}" package="Root::#{mlmName'}" parents=""/>\n|]
    TagInstance -> case cOf of
      Just cOf' ->
        [i|    <addInstance abstract="#{cIsAbstract}" name="#{cName'}" of="Root::#{mlmName'}::#{cName cOf'}" package="Root::#{mlmName'}" parents=""/>\n|]
      _ ->
        ""
    TagChangeOneParent ->
      [i|Root::#{mlmName'}::#{cName'},|]

-- Parents
instance XModelerable Name Class [Class] where
  get _ _ [] = ""
  get mlmName' (Class {cName}) parents =
    [i|    <changeParent class="Root::#{mlmName'}::#{cName}" new="#{doParents}" old="" package="Root::#{mlmName'}"/>\n|]
    where
      doParents =
        init (concatMap (get mlmName' TagChangeOneParent) parents)

-- Multiplicity
instance XModelerable () () Multiplicity where
  get () () (Multiplicity {lower, upper}) =
    [i|Seq{#{lower},#{upper},#{show (upper /= -1)},false}|]

-- Attribute
instance XModelerable Name () Attribute where
  get mlmName' () (Attribute {tLevel, tName, tType, tClass, tMultiplicity}) =
    [i|    <addAttribute class="Root::#{mlmName'}::#{cName tClass}" level="#{tLevel}" multiplicity="#{get () () tMultiplicity}" name="#{tName}" package="Root::#{mlmName'}" type="Root::#{get () () tType}"/>\n|]

-- Operation
instance XModelerable Name () Operation where
  get mlmName' () (Operation {oBody, oLevel, oIsMonitored, oName, oType, oClass}) =
    [i|    <addOperation body="#{oBody}" class="Root::${mlmName'}::#{cName oClass}" level="#{oLevel}" monitored="#{oIsMonitored}" name="#{oName}" package="Root::#{mlmName'}" paramNames="" paramTypes="" type="Root::#{get () () oType}"/>\n|]

-- Slot
instance XModelerable Name () Slot where
  get mlmName' () (Slot {sAttribute, sValue, sClass}) =
    [i|    <changeSlotValue class="Root::#{mlmName'}::#{cName sClass}" package="Root::#{mlmName'}" slotName="#{tName sAttribute}" valueToBeParsed="#{get () () sValue}"/>\n|]

-- Association
instance XModelerable Name () Association where
  get mlmName' () (Association {aName, aSource, aTarget, aLvlSource, aLvlTarget, aMultSourceToTarget, aMultTargetToSource, aSourceVisibleFromTarget, aTargetVisibleFromSource}) =
    [i|    <addAssociation accessSourceFromTargetName="#{aName}_#{from}" accessTargetFromSourceName="#{aName}_#{to}" classSource="Root::#{mlmName'}::#{from}" classTarget="Root::#{mlmName'}::#{to}" fwName="#{aName}" instLevelSource="#{aLvlSource}" instLevelTarget="#{aLvlTarget}" isSymmetric="false" isTransitive="false" aMultSourceToTarget="#{get () () aMultSourceToTarget}" aMultTargetToSource="#{get () ()  aMultTargetToSource}" package="Root::#{mlmName'}" reverseName="-1" aSourceVisibleFromTarget="#{aSourceVisibleFromTarget}" aTargetVisibleFromSource="#{aTargetVisibleFromSource}"/>\n|]
   where
     from = cName aSource
     to = cName aTarget

-- Link
instance XModelerable Name () Link where
  get mlmName' () (Link {lSource, lTarget, lAssociation}) =
    [i|    <addLink classSource="Root::#{mlmName'}::#{cName lSource}" classTarget="Root::#{mlmName'}::#{cName lTarget}" name="#{aName lAssociation}" package="Root::#{mlmName'}"/>\n|]

-- MLM
instance XModelerable [Object] (Double, Int) MLM where
  get mlmObjects (xx, txTy) (MLM {mlmName, mlmClasses, mlmAssociations, mlmLinks}) =
    let
      allTagsObject = concatMap (get mlmName ()) mlmObjects
      allTagsMetaClass = concatMap (get mlmName TagMetaClass) mlmClasses
      allTagsInstance = concatMap (get mlmName TagInstance) mlmClasses
      allTagsChangeParents =
        concatMap
          (\class' -> get mlmName class'(cParents class'))
          mlmClasses
      allTagsAttribute =
        concatMap
          (concatMap (get mlmName ()) . cAttributes)
          mlmClasses
      allTagsOperation =
        concatMap
          (concatMap (get mlmName ()) . cOperations)
          mlmClasses
      allTagsSlot =
        concatMap
          (concatMap (get mlmName ()) . cSlots)
          mlmClasses
      allTagsAssociation = concatMap (get mlmName ()) mlmAssociations
      allTagsLink = concatMap (get mlmName ()) mlmLinks
    in
      [i|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<XModeler>
  <Version>2</Version>
  <Categories/>
  <Projects>
    <Project name="Root::#{mlmName}"/>
  </Projects>
  <Diagrams>
    <Diagram label="#{mlmName}_diagram" package_path="Root::#{mlmName}" showConstraintReports="true" showConstraints="true" showDerivedAttributes="true" showDerivedOperations="true" showGettersAndSetters="true" showMetaClassName="false" showOperationValues="true" showOperations="true" showSlots="true">
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
              mlm@(MLM {mlmClasses, mlmAssociations, mlmLinks}) = let
    vertices =
      map cName mlmClasses :: [Name]
    extractEdge from to x =
      (cName (from x), cName (to x), ()) :: (String, String, ())
    edges =
      map (extractEdge aSource aTarget) mlmAssociations ++
      map (extractEdge lSource lTarget) mlmLinks ++
      concatMap
        (\child -> map
          (\parent -> (cName child, cName parent, ()))
          (cParents child))
        mlmClasses :: [(String, String, ())]

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
    return $ get objects (xx, txTy) mlm