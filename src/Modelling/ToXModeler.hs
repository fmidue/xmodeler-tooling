{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Modelling.ToXModeler (toXModeler) where

import Data.String.Interpolate ( i )
import Diagrams.Points (Point (P))
import Diagrams.TwoD.Types (V2 (V2))
import Diagrams.TwoD.GraphViz (layoutGraph, mkGraph, getGraph)
import Data.GraphViz (GraphvizCommand)
import Data.Map.Strict (toList)
import Data.Bifunctor (first, second)
import Data.Char (toLower)
import Modelling.Types (
  MetaClass (..),
  Instance (..),
  ChangeParent (..),
  Attribute (..),
  Operation (..),
  ChangeSlotValue (..),
  Association (..),
  Link (..),
  MLM (..),
  getType,
  getValue
  )

smallify :: Bool -> String
smallify = map toLower . show

getObjectsCoordinates :: GraphvizCommand -> (Double -> Double) -> [String] -> [(String, String,())] -> IO [(String, (Int, Int))]
getObjectsCoordinates layoutCommand spaceOut vertices edges =
  do g <- layoutGraph layoutCommand $ mkGraph vertices edges
     return $ map (\(vertex, P (V2 x y)) -> (vertex, (adjust x, adjust y))) $ toList $ fst $ getGraph g
  where adjust = round . spaceOut

{-
it takes a list of names of classes (capital letters) and a list of names of instances of these classes
(small letters followed by a number) and returns a list of the tag Object concatenated to become a String.
-}
addObjects :: [(String, (Int, Int))] -> String -> String
addObjects objectsCoordinates projectName =
  concat [[i|        <Object hidden="false" ref="Root::#{projectName}::#{objectName}" x="#{x}" y="#{y}"/>\n|]
  | (objectName, (x, y)) <- objectsCoordinates]

addMetaClasses :: [MetaClass] -> String -> String
addMetaClasses metaClasses projectName = concat [[i|    <addMetaClass abstract="#{smallify (mAbstract x)}" level="#{mLevel x}" name="#{mName x}" package="Root::#{projectName}" parents=""/>\n|]
  | x <- metaClasses]

addInstances :: [Instance] -> String -> String
addInstances instances projectName = concat [[i|    <addInstance abstract="#{smallify (iAbstract x)}" name="#{iName x}" of="Root::#{projectName}::#{iOf x}" package="Root::#{projectName}" parents=""/>\n|]
  | x <- instances]

-- The function doParents is needed because the value of the attribute "new" could have multiple class from which it inherits.
-- The purpose of "init" is to remove the last "," from the list of parents that is produced on the right side of the definition of .
doChangeParents :: [ChangeParent] -> String -> String
doChangeParents parentsChanges projectName = concat [[i|    <changeParent class="Root::#{projectName}::#{pClass x}" new="#{doParents (pNew x)}" old="" package="Root::#{projectName}"/>\n|]
  | x <- parentsChanges]
    where
      doParents :: [String] -> String
      doParents parents = init (concat [[i|Root::#{projectName}::#{p},|] | p <- parents ])

--The representation of infinity (having no upper limit in a multiplicity) in XModeler is "-1".
addAssociations :: [Association] -> String -> String
addAssociations associations projectName = concat [[i|    <addAssociation accessSourceFromTargetName="#{sFwName x}_#{sSource x}" accessTargetFromSourceName="#{sFwName x}_#{sTarget x}" classSource="Root::#{projectName}::#{sSource x}" classTarget="Root::#{projectName}::#{sTarget x}" fwName="#{sFwName x}" instLevelSource="#{sInstLevelSource x}" instLevelTarget="#{sInstLevelTarget x}" isSymmetric="false" isTransitive="false" multSourceToTarget="Seq{#{sMultSourceToTargetMin x},#{sMultSourceToTargetMax x},#{smallify (sMultSourceToTargetMax x /= -1)},false}" multTargetToSource="Seq{#{sMultTargetToSourceMin x},#{sMultTargetToSourceMax x},#{smallify (sMultTargetToSourceMax x /= -1)},false}" package="Root::#{projectName}" reverseName="-1" sourceVisibleFromTarget="#{smallify (sSourceVisibleFromTarget x)}" targetVisibleFromSource="#{smallify (sTargetVisibleFromSource x)}"/>\n|] | x <- associations]

addLinks :: [Link] -> String -> String
addLinks links projectName = concat [[i|    <addLink classSource="Root::#{projectName}::#{lClassSource x}" classTarget="Root::#{projectName}::#{lClassTarget x}" name="#{lName x}" package="Root::#{projectName}"/>\n|]
  | x <- links]

addAttributes :: [Attribute] -> String -> String
addAttributes changeSlotValues projectName = concat [[i|    <addAttribute class="Root::#{projectName}::#{aClass x}" level="#{aLevel x}" multiplicity="Seq{#{aMultiplicityMin x},#{aMultiplicityMax x},#{smallify (aMultiplicityMax x /= -1)},false}" name="#{aName x}" package="Root::#{projectName}" type="Root::#{getType (aType x)}"/>\n|] | x <- changeSlotValues]

addOperations :: [Operation] -> String -> String
addOperations operations projectName = concat [[i|    <addOperation body="#{oBody x}" class="Root::#{projectName}::#{oClass x}" level="#{oLevel x}" monitored="false" name="#{oName x}" package="Root::#{projectName}" paramNames="" paramTypes="" type="Root::#{getType (oType x)}"/>\n|] | x <- operations]

doChangeSlotValues :: [ChangeSlotValue] -> String -> String
doChangeSlotValues changeSlotValues projectName = concat [[i|    <changeSlotValue class="Root::#{projectName}::#{vClass x}" package="Root::#{projectName}" slotName="#{vName x}" valueToBeParsed="#{getValue (vValueToBeParsed x)}"/>\n|] | x <- changeSlotValues]

getRange :: [Int] -> Double
getRange x = fromIntegral $ maximum x - minimum x

toXModeler :: GraphvizCommand -> (Double -> Double) -> Double -> Int -> MLM -> IO String
toXModeler layoutCommand spaceOut scaleFactor extraOffset mlm =
  let
    projectName = mlmName mlm
    metaClasses = mlmMetaClasses mlm :: [MetaClass]
    instances = mlmInstances mlm :: [Instance]
    changeParents = mlmChangeParents mlm :: [ChangeParent]
    attributes = mlmAttributes mlm :: [Attribute]
    operations = mlmOperations mlm :: [Operation]
    changeSlotValues = mlmChangeSlotValues mlm :: [ChangeSlotValue]
    associations = mlmAssociations mlm ::[Association]
    links = mlmLinks mlm :: [Link]
    vertices = map mName metaClasses ++ map iName instances :: [String]
    edges = map (\x -> (sSource x, sTarget x, ())) associations :: [(String, String, ())]
  in
   do allCoordinates <- getObjectsCoordinates layoutCommand spaceOut vertices edges
      let allCoordinates' = map (second (first (+ extraOffset))) allCoordinates
      let addObjects' = addObjects allCoordinates' projectName
      let xs = map (fst . snd) allCoordinates
      let ys = map (snd . snd) allCoordinates
      let xx = log $ max (getRange xs) (getRange ys) * scaleFactor / 1000 :: Double
      let txTy = round $ 50 * xx :: Int
      return [i|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
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
#{addObjects'}
      </Objects>
      <Edges/>
      <Labels/>
      <Preferences/>
      <View name="Main View" tx="#{txTy}" ty="#{txTy}" xx="#{xx}"/>
    </Diagram>
  </Diagrams>
  <Logs>
#{addMetaClasses metaClasses projectName}
#{addInstances instances projectName}
#{doChangeParents changeParents projectName}
#{addAttributes attributes projectName}
#{addOperations operations projectName}
#{doChangeSlotValues changeSlotValues projectName}
#{addAssociations associations projectName}
#{addLinks links projectName}
  </Logs>
</XModeler>|]
