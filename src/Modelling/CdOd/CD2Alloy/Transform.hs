{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Modelling.CdOd.CD2Alloy.Transform (
  Parts {- only for legacy-apps: -} (..),
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  ) where

import Modelling.CdOd.Types (
  Association,
  AssociationType (..),
  ObjectConfig (..),
  )

import Data.Bifunctor                   (first)
import Data.List                        (intercalate, union)
import Data.FileEmbed                   (embedStringFile)
import Data.Maybe                       (catMaybes, fromMaybe, isJust)
import Data.String.Interpolate          (i)

{-|
Parts belonging to the CD2Alloy Alloy program.
-}
data Parts = Parts {
  part1 :: !String,
  part2 :: !String,
  part3 :: !String,
  part4 :: !String
  }

transform
  :: ([(String, Maybe String)], [Association])
  -> ObjectConfig
  -> Maybe Bool
  -> Bool
  -> String
  -> String
  -> Parts
transform
  (classes, associations)
  objectConfig
  hasSelfLoops
  noIsolationLimitation
  index
  time =
  Parts { part1, part2, part3, part4 }
  where
    template :: String
    template = $(embedStringFile "alloy/od/template.als")
    part1 :: String
    part1 = [i|
// Alloy Model for CD#{index}
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: #{time}

module umlp2alloy/CD#{index}Module

#{template}
#{objectsFact}
#{limitLinks}
#{loops}
///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////
|]
    objectsFact :: String
    objectsFact
      | noIsolationLimitation
      = noEmptyInstances
      | otherwise
      = limitIsolatedObjects
    limitIsolatedObjects = [i|
fact LimitIsolatedObjects {
  \#Obj > mul[2, \#{o : Obj | no o.get and no get.o}]
}
|]
    noEmptyInstances = [i|
fact NonEmptyInstancesOnly {
  some Obj
}
|]
    withJusts f xs
      | any isJust xs = f $ catMaybes xs
      | otherwise     = ""
    limitLinks = withJusts (\ps -> [i|
fact LimitLinks {
#{unlines ps}
}
|]) [
      ("  #Obj >= " ++) . show <$> mlower 1 (objects objectConfig),
      ("  #get >= " ++) . show <$> mlower 0 (links objectConfig),
      ("  #get <= " ++) . show <$> snd (links objectConfig),
      uncurry linksPerObjects $ first (mlow 0) $ linksPerObject objectConfig]
    linksPerObjects Nothing Nothing = Nothing
    linksPerObjects mmin mmax = Just $
      "  all o : Obj | let x = plus[#o.get,minus[#get.o,#o.get.o]] |"
      ++ maybe "" ((" x >= " ++) . show) mmin
      ++ maybe "" (const " &&") (mmin >> mmax)
      ++ maybe "" ((" x <= " ++) . show) mmax
    mlower l = mlow l . fst
    mlow l x = if x <= l then Nothing else Just x
    part2 = [i|
// Concrete names of fields
#{unlines (associationSigs associations)}
|]
    part3 = [i|
// Classes
#{unlines (classSigs classNames)}
|]
    part4 = [i|
///////////////////////////////////////////////////
// CD#{index}
///////////////////////////////////////////////////

// Types wrapping subtypes
#{unlines (subTypes index classesWithDirectSubclasses)}
// Types wrapping field names
#{unlines (fieldNames index associations classes)}
// Types wrapping composite structures and field names
#{if noCompositions then "" else unlines (compositesAndFieldNames index compositions classes)}
// Properties
#{predicate index associations classNames}
|]
    classNames = map fst classes
    classesWithDirectSubclasses =
      map (\(name, _) -> (name, map fst (filter ((== Just name) . snd) classes))) classes
    noCompositions = null compositions
    compositions = filter (\(a,_,_,_,_,_) -> a == Composition) associations
    loops            = case hasSelfLoops of
      Nothing    -> ""
      Just True  -> [i|
fact SomeSelfLoops {
  some o : Obj | o in o.get[FName]
}|]
      Just False -> [i|
fact NoSelfLoops {
  no o : Obj | o in o.get[FName]
}|]

createRunCommand :: String -> Int -> ObjectConfig -> String
createRunCommand command numClasses objectConfig = [i|
///////////////////////////////////////////////////
// Run commands
///////////////////////////////////////////////////

run { #{command} } for #{maxObjects} Obj, #{intSize} Int
|]
  where
    maxObjects = snd $ objects objectConfig
    intSize :: Int
    intSize = ceiling intSize'
    intSize' :: Double
    intSize' = logBase 2 $ fromIntegral $ 2 * maxInt + 1
    maxInt = maximum [
      numClasses * maxObjects,
      2 * maxObjects,
      count links,
      count linksPerObject
      ]
    count f = fromMaybe (fst $ f objectConfig) $ snd (f objectConfig)

associationSigs :: [Association] -> [String]
associationSigs = map (\(_,name,_,_,_,_) -> "one sig " ++ name ++ " extends FName {}")

classSigs :: [String] -> [String]
classSigs = map (\name -> "sig " ++ name ++ " extends Obj {}")

subTypes :: String -> [(String, [String])] -> [String]
subTypes index = concatMap (\(name, directSubclasses) ->
  [ "fun " ++ name ++ subsCD ++ " : set Obj {"
  , "  " ++ intercalate " + " (name : map (++ subsCD) directSubclasses)
  , "}"
  ])
  where
    subsCD = "SubsCD" ++ index

fieldNames :: String -> [Association] -> [(String, Maybe String)] -> [String]
fieldNames index associations = concatMap (\(this, super) ->
  let thisAssociations = filter (\(_,_,_,from,_,_) -> from == this) associations
  in [ "fun " ++ this ++ fieldNamesCD ++" : set FName {"
     , "  " ++ intercalate " + " (maybe "none" (++ fieldNamesCD) super
                                  : map (\(_,name,_,_,_,_) -> name) thisAssociations)
     , "}"
     ])
  where
    fieldNamesCD = "FieldNamesCD" ++ index

compositesAndFieldNames :: String -> [Association] -> [(String, Maybe String)] -> [String]
compositesAndFieldNames index compositions = concatMap (\(this, super) ->
  let thisCompositions = filter (\(_,_,_,_,to,_) -> to == this) compositions
  in [ "fun " ++ this ++ compositesCD ++ " : set Obj {"
     , "  " ++ intercalate " + " (maybe "none" (++ compositesCD) super
                                  : map (\(_,_,_,from,_,_) -> from ++ subsCD) thisCompositions)
     , "}"
     , "fun " ++ this ++ compFieldNamesCD ++ " : set FName {"
     , "  " ++ intercalate " + " (maybe "none" (++ compFieldNamesCD) super
                                  : map (\(_,name,_,_,_,_) -> name) thisCompositions)
     , "}"
     ])
  where
    compositesCD = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD = "SubsCD" ++ index

predicate :: String -> [Association] -> [String] -> String
predicate index associations classNames = [i|
pred cd#{index} {

  Obj = #{intercalate " + " classNames}

  // Contents
#{unlines objFNames}
  // Associations
#{unlines objAttribs}
  // Compositions
#{if anyCompositions then unlines compositions else ""}
}
|]
  where
    objFNames = map (\name -> [i|  ObjFNames[#{name}, #{name}#{fieldNamesCD}]|]) classNames
    objAttribs = concatMap
      (\(_, name, mult1, class1, class2, mult2) ->
          [makeAssoc "Attrib" class1 name class2 mult2
          , makeAssoc "" class2 name class1 mult1])
      associations
    makeAssoc
      :: Show a
      => String -> String -> String -> String -> (a, Maybe a) -> String
    makeAssoc att from name to (low, Nothing) =
      [i|  ObjL#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}]|]
    makeAssoc att from name to (low, Just up) =
      [i|  ObjLU#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}, #{show up}]|]
    anyCompositions = any (\(a,_,_,_,_,_) -> a == Composition) associations
    compositions = map
      (\name -> [i|  Composition[#{name}#{compositesCD}, #{name}#{compFieldNamesCD}, #{name}]|])
      classNames
    fieldNamesCD     = "FieldNamesCD" ++ index
    compositesCD     = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD           = "SubsCD" ++ index

mergeParts
  :: Parts
  -> Parts
  -> Parts
mergeParts p p' = Parts
  (part1 p)
  (part2 p `unionL` part2 p')
  (part3 p `unionL` part3 p')
  (part4 p ++ part4 p')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y

combineParts :: Parts -> String
combineParts Parts {..} = part1 ++ part2 ++ part3 ++ part4
