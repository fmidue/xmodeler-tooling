{-# LANGUAGE QuasiQuotes, FunctionalDependencies #-}

module Modelling.MLM.ToXModeler (toXModeler) where

import Data.Char (ord)
import Data.Map.Strict (toList)
import Data.String.Interpolate (i)
import Data.GraphViz (GraphvizCommand)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Diagrams.Points (Point (P))
import Diagrams.TwoD.Types (V2 (V2))
import Diagrams.TwoD.GraphViz (layoutGraph, mkGraph, getGraph)
import Numeric (showFFloat)
import GHC.Float (double2Float)

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
  Value (..),
  Name (..),
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
}

class XModelerable context a | a -> context where
  get :: context -> a -> String

-- Name : instead of an XModelerable () instance,
nameString :: Name -> String
nameString (Name name) = name

-- Bool
instance XModelerable () Bool where
  get () True = "true"
  get () False = "false"

-- Float
instance XModelerable () Float where
  get () x = showFFloat (Just 2) x ""

-- Type
instance XModelerable () Type where
  get () t = let
    cor = "XCore::" ++ show t
    aux = "Auxiliary::" ++ show t
    in case t of
      Boolean -> cor
      Integer -> cor
      Float -> cor
      String -> cor
      Element -> cor
      _ -> aux

-- Value
instance XModelerable () Value where
  get () v = case v of
    VBoolean b -> get () b
    VInteger i' -> show i'
    VFloat f -> get () f
    VString s -> [i|#{map ord s}.asString()|]
    VMonetaryValue (amount, currency) ->
      [i|Auxiliary::MonetaryValue(#{amount}, Auxiliary::Currency(&quot;#{currency}&quot;, &quot;#{currency}&quot;, 1.0))|]
    VDate (year, month, day) ->
      [i|Auxiliary::Date::createDate(#{year}, #{month}, #{day})|]
    VCurrency c ->
      [i|Auxiliary::Currency(&quot;#{currencySymbol c}&quot;, &quot;#{c}&quot;, #{get () (relativeToEur c)})|]
    VElement e -> e
    VAuxiliaryClass c -> c
    VComplex x -> x

-- Object
instance XModelerable Name Object where
  get (Name projectName) Object{name, x, y} =
    [i|        <Object hidden="false" ref="Root::#{projectName}::#{nameString name}" x="#{x}" y="#{y}"/>\n|]

-- Class
instance XModelerable Name AsUnclassified where
  get (Name projectName) (AsUnclassified Class{isAbstract, level, name, classifier = Nothing}) =
    [i|    <addMetaClass abstract="#{get () isAbstract}" level="#{level}" name="#{nameString name}" package="Root::#{projectName}" parents=""/>\n|]
  get _ _ = error "ERROR! This class is supposed to have no classifier, but it seems to have one!"

--  Class : Instance
instance XModelerable Name AsInstance where
  get (Name projectName) (AsInstance Class{isAbstract, name, classifier = Just (Name classifierName)}) =
    [i|    <addInstance abstract="#{get () isAbstract}" name="#{nameString name}" of="Root::#{projectName}::#{classifierName}" package="Root::#{projectName}" parents=""/>\n|]
  get _ _ = error "ERROR! This class is supposed to have a classifier, but it seems to have none!"

-- Class : ChangeParents (Child)
instance XModelerable Name AsChild where
  get (Name projectName) (AsChild Class{name, parents}) =
    [i|    <changeParent class="Root::#{projectName}::#{nameString name}" new="#{doParents}" old="" package="Root::#{projectName}"/>\n|]
      where doParents = init (concatMap (\p -> [i|Root::#{projectName}::#{nameString p},|]) parents)

-- Multiplicity
instance XModelerable () Multiplicity where
  get () (Multiplicity (lower, upper)) =
    [i|Seq{#{lower},#{fromMaybe (-1) upper},#{get () (isJust upper)},false}|]

-- Attribute
instance XModelerable (Name, Name) Attribute where
  get (Name projectName, Name className) Attribute{level, name, dataType, multiplicity} =
    [i|    <addAttribute class="Root::#{projectName}::#{className}" level="#{level}" multiplicity="#{get () multiplicity}" name="#{nameString name}" package="Root::#{projectName}" type="Root::#{get () dataType}"/>\n|]

-- Operation
instance XModelerable (Name, Name) Operation where
  get (Name projectName, Name className) Operation{body, level, isMonitored, name, dataType} =
    [i|    <addOperation body="#{body}" class="Root::#{projectName}::#{className}" level="#{level}" monitored="#{get () isMonitored}" name="#{nameString name}" package="Root::#{projectName}" paramNames="" paramTypes="" type="Root::#{get () dataType}"/>\n|]

-- Slot
instance XModelerable (Name, Name) Slot where
  get (Name projectName, Name slotClass) Slot{name, value} =
    [i|    <changeSlotValue class="Root::#{projectName}::#{slotClass}" package="Root::#{projectName}" slotName="#{nameString name}" valueToBeParsed="#{get () value}"/>\n|]

-- Association
instance XModelerable Name Association where
  get (Name projectName) Association{name, source, target, lvlSource, lvlTarget, multTarget, multSource, visibleSource, visibleTarget} =
    [i|    <addAssociation accessSourceFromTargetName="#{nameString name}_#{from}" accessTargetFromSourceName="#{nameString name}_#{to}" classSource="Root::#{projectName}::#{from}" classTarget="Root::#{projectName}::#{to}" fwName="#{nameString name}" instLevelSource="#{lvlSource}" instLevelTarget="#{lvlTarget}" isSymmetric="false" isTransitive="false" multSourceToTarget="#{get () multTarget}" multTargetToSource="#{get () multSource}" package="Root::#{projectName}" reverseName="-1" sourceVisibleFromTarget="#{get () visibleSource}" targetVisibleFromSource="#{get () visibleTarget}"/>\n|]
   where
     from = nameString source
     to = nameString target

-- Link
instance XModelerable Name Link where
  get (Name projectName) Link{source, target, name} =
    [i|    <addLink classSource="Root::#{projectName}::#{nameString source}" classTarget="Root::#{projectName}::#{nameString target}" name="#{nameString name}" package="Root::#{projectName}"/>\n|]

-- MLM
instance XModelerable ([Object], Double, Int) MLM where
  get (mlmObjects, xx, txTy) MLM{name = projectName, classes, associations, links} =
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
    <Project name="Root::#{nameString projectName}"/>
  </Projects>
  <Diagrams>
    <Diagram label="#{nameString projectName}_diagram" package_path="Root::#{nameString projectName}" showConstraintReports="false" showConstraints="false" showDerivedAttributes="false" showDerivedOperations="false" showGettersAndSetters="false" showMetaClassName="false" showOperationValues="true" showOperations="true" showSlots="true">
      <Categories/>
      <Owners/>
      <Objects>
#{allTagsObject}
      </Objects>
      <Edges/>
      <Labels/>
      <Preferences/>
      <View name="Main View" tx="#{txTy}" ty="#{txTy}" xx="#{get () (double2Float xx)}"/>
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
              mlm@MLM{classes, associations, links} = let
    vertices = map (nameString . #name) classes :: [String]
    extractEdge from to x =
      (nameString (from x), nameString (to x), ()) :: (String, String, ())
    edges =
      map (extractEdge #source #target) associations ++
      map (extractEdge #source #target) links ++
      concatMap
        (\child -> map
          (\parent -> (nameString (#name child), nameString parent, ()))
          (#parents child))
        classes :: [(String, String, ())]

    adjust :: Double -> Int
    adjust = round . spaceOut

    extractObject (vertex, P (V2 x y)) = Object (Name vertex) (extraOffset + adjust x) (adjust y)
  in do
    g <- layoutGraph layoutCommand $ mkGraph vertices edges
    let objects = map extractObject $ toList $ fst $ getGraph g :: [Object]
    let xx = 1 / sqrt ( sqrt ( sqrt ( fromIntegral (length classes)))) * scaleFactor :: Double
    let xxTruncated = round (xx * 100 :: Double) :: Int
    putStrLn $ "view scale = " ++ show xxTruncated ++ "%"
    let txTy = extraOffset * round xx :: Int
    return $ get (objects, xx, txTy) mlm
