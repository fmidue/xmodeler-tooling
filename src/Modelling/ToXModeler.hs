{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Modelling.CdOd.ToXModeler (toXModeler) where

import Data.String.Interpolate ( i )
import Data.Char (toLower, isDigit)
import Data.Maybe (fromMaybe)
import Modelling.CdOd.Types (Od, Syntax)
import qualified Modelling.CdOd.Types as AssociationType (AssociationType(..))
import Diagrams.Points (Point (P))
import Diagrams.TwoD.Types (V2 (V2))
import Diagrams.TwoD.GraphViz (layoutGraph, mkGraph, getGraph)
import Data.GraphViz (GraphvizCommand)
import Data.Map.Strict (toList)
import Data.Bifunctor (first, second)

projectName :: String
projectName = "projectName"
diagramName :: String
diagramName = "diagramName"
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


getObjectsCoordinates :: GraphvizCommand -> (Double -> Double) -> [String] -> [(String, String,())] -> IO [(String, (Int, Int))]
getObjectsCoordinates layoutCommand spaceOut vertices edges =
  do g <- layoutGraph layoutCommand $ mkGraph vertices edges
     return $ map (\(vertex, P (V2 x y)) -> (vertex, (adjust x, adjust y))) $ toList $ fst $ getGraph g
  where adjust = round . spaceOut

{-
it takes a list of names of classes (capital letters) and a list of names of instances of these classes
(small letters followed by a number) and returns a list of the tag Object concatenated to become a String.
-}
addObjects :: [(String, (Int, Int))] -> String
addObjects objectsCoordinates =
  concat [[i|        <Object hidden="false" ref="Root::#{projectName}::#{objectName}" x="#{x}" y="#{y}"/>\n|]
  | (objectName, (x, y)) <- objectsCoordinates]

addMetaClasses :: [MetaClass] -> String -> String
addMetaClasses metaClasses projectName = concat [[i|    <addMetaClass abstract="#{smallify (mAbstract x)}" level="#{mLevel x}" name="#{mName x}" package="Root::#{projectName}" parents=""/>\n|]
  | x <- metaClasses]

addInstances :: [Instance] -> String -> String
addInstances instances projectName = concat [[i|    <addInstance abstract="#{smallify (iAbstract x)}" name="#{iName x}" of="Root::#{projectName}::#{iOf x}" package="Root::#{projectName}" parents=""/>\n|]
  | x <- instances]

the function doParents is needed because the value of the attribute "new" could have multiple class names with each preceded by some text.

The purpose of "init" is to remove the last "," from the list of parents that is produced on the right side of the definition of doParents.
-}
changeParents :: [(String,[String], ())] -> String
changeParents parentsChanges = concat [[i|    <changeParent class="Root::#{projectName}::#{child}" new="#{doParents parents}" old="" package="Root::#{projectName}"/>\n|]
  | (child, parents, ()) <- parentsChanges]
    where
      doParents :: [String] -> String
      doParents parents = init (concat [[i|Root::#{projectName}::#{p},|] | p <- parents ])

matchAssociation :: (AssociationType.AssociationType,
  String, (Int, Maybe Int), String, String, (Int, Maybe Int)) ->
  ((String, Int, Int, Int, Int), (String, String, ()))
matchAssociation (AssociationType.Association,
  name, (sourceMin, sourceMax), source, target, (targetMin, targetMax)) =
    ((name, sourceMin, fromMaybe (-1) sourceMax, targetMin, fromMaybe (-1) targetMax),
    (source, target, ()))
matchAssociation (_,
  name, (targetMin, targetMax), target, source, (sourceMin, sourceMax)) =
    ((name, sourceMin, fromMaybe (-1) sourceMax, targetMin, fromMaybe (-1) targetMax),
    (source, target, ()))

{-
This function takes a list of Associations (see src\Modelling\CdOd\Types.hs) and returns a list of the tag
addAssociation concatenated to become a String.

The representation of infinity (having no upper limit in a multiplicity) in XModeler is "-1" as opposed to it
in Autotool which is "Nothing". This is why I used fromMaybe.

The values for the attributes accessSourceFromTargetName and accessTargetFromSourceName are the same as the
names of source class and target class, but in small letters.
-}
addAssociations :: [((String, Int, Int, Int, Int), (String, String, ()))] -> String
addAssociations = concatMap addAssociation
  where
    addAssociation :: ((String, Int, Int, Int, Int), (String, String, ())) -> String
    addAssociation ((name, sourceMin, sourceMax, targetMin, targetMax), (source, target, ()))
        = [i|    <addAssociation accessSourceFromTargetName="#{name}_#{map toLower source}" accessTargetFromSourceName="#{name}_#{map toLower target}" classSource="Root::#{projectName}::#{source}" classTarget="Root::#{projectName}::#{target}" fwName="#{name}" instLevelSource="0" instLevelTarget="0" isSymmetric="false" isTransitive="false" multSourceToTarget="Seq{#{targetMin},#{targetMax},#{targetMax /= -1},false}" multTargetToSource="Seq{#{sourceMin},#{sourceMax},#{sourceMax /= -1},false}" package="Root::#{projectName}" reverseName="-1" sourceVisibleFromTarget="false" targetVisibleFromSource="true"/>\n|]

addLinks :: [(String, (String, String, ()))] -> String
addLinks links = concat [[i|    <addLink classSource="Root::#{projectName}::#{source}" classTarget="Root::#{projectName}::#{target}" name="#{name}" package="Root::#{projectName}"/>\n|]
  | (name, (source, target, ())) <- links]

{-
  in the let-in part the data is extracted and "pre-processed".
  Translation from Autotool encoding to a structure that has only the parts significant to XModeler and in somewhat helpful order:
-}
toXModeler :: GraphvizCommand -> GraphvizCommand -> (Double -> Double) -> Double -> Int -> Syntax -> Od -> IO String
toXModeler cdLayoutCommand odLayoutCommand spaceOut scaleFactor extraOffset cd od =
  let
    -- classes we have in the class diagram (metaclasses):
    classesNames = map fst (fst cd) :: [String]

    -- classes names paired with parents, only if the list of parents is not empty:
    parentsChanges = map (uncurry ( , , ())) $ filter (not . null . snd) (fst cd) :: [(String, [String], ())]

    -- associations between these classes:
    associations = map matchAssociation (snd cd) :: [((String, Int, Int, Int, Int), (String, String, ()))]
    -- instances as they are given, just without the separator ($) :
    instances0 = map (filter (/='$')) (fst od) :: [String]

    --  the class names that these instances are instantiating:
    instancesClasses = map (filter (not . isDigit)) instances0 :: [String]

    -- the names of the instances (they have to be small letters for XModeler):
    instancesNames = map (map toLower) instances0 :: [String]

    -- pairs of an instance name with the name of the class that it instantiates
    -- for example : [("c0","C"),("b0","B"),("b1","B"),("b2","B")] :
    instances = zip instancesNames instancesClasses :: [(String,String)]

    -- links. The encoding uses the indices of the instances instead of their names
    links = map (\(source, target, linkName) -> (linkName, (instancesNames !! source, instancesNames !! target, ()))) (snd od)
      :: [(String, (String, String, ()))]

    cdEdges = map snd associations ++
      concatMap (\(child, parents, ()) -> map (child, ,()) parents) parentsChanges
        :: [(String,String,())]
    odEdges = map snd links :: [(String,String,())]

    getRange :: [Int] -> Double
    getRange x = fromIntegral $ maximum x - minimum x
  in
   do cdCoordinates <- getObjectsCoordinates cdLayoutCommand spaceOut classesNames cdEdges
      odCoordinates0 <- getObjectsCoordinates odLayoutCommand spaceOut instancesNames odEdges
      let offset = extraOffset + maximum (map (fst . snd) cdCoordinates)
      let odCoordinates = map (second (first (+ offset))) odCoordinates0
      let allCoordinates = cdCoordinates ++ odCoordinates
      let addObjects' = addObjects allCoordinates
      let xs = map (fst . snd) allCoordinates
      let ys = map (snd . snd) allCoordinates
      let xx = log $ (max (getRange xs) (getRange ys)) * scaleFactor / 1000 :: Double
      let txTy = round $ 50 * xx :: Int
      return [i|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<XModeler>
  <Version>2</Version>
  <Categories/>
  <Projects>
    <Project name="Root::#{projectName}"/>
  </Projects>
  <Diagrams>
    <Diagram label="#{diagramName}" package_path="Root::#{projectName}" showDerivedAttributes="true" showDerivedOperations="true" showGettersAndSetters="false" showOperationValues="true" showOperations="true" showSlots="true">
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
#{addMetaClasses classesNames}
#{addInstances instances}
#{changeParents parentsChanges}
#{addAssociations associations}
#{addLinks links}
  </Logs>
</XModeler>|]
