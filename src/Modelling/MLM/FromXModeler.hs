module Modelling.MLM.FromXModeler (fromXModeler) where

import Modelling.MLM.Types (
  MLM (..),
  Name (..),
  Level,
  Class (..),
  Multiplicity (..),
  Type (..),
  Attribute (..),
  Slot (..),
  Operation (..),
  OperationBody (..),
  XModelerCurrency (..),
  emptyClass,
  emptyAttribute,
  emptyAssociation,
  emptyLink,
  emptyType,
  emptyName,
  emptyOperation
  )

import Text.XML.HXT.Core (readString, runX, hasName, getAttrValue, withParseHTML, withWarnings, no, (//>), (>>>))
import Modelling.MLM.Modify ((<<<),(|<<<|), SourceOrTarget (..))
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (second)
import Data.Char (toUpper, chr)
import Text.Read (readMaybe)

data RawSlot = RawSlot {
  slotName :: Name,
  slotType :: Type,
  rawValue :: String,
  slotClass :: Name
}

readCarefully :: Read a => String -> (String -> a)
readCarefully x = fromMaybe (error x) . readMaybe

emptyRawSlot :: RawSlot
emptyRawSlot = RawSlot emptyName emptyType "" emptyName

accumulate :: [a] -> [b] -> ([a] -> b -> a) -> [a]
accumulate start list f = foldl (\soFar x -> soFar ++ [f soFar x]) start list

accumulateSimple :: [b] -> (b -> a) -> [a]
accumulateSimple list f = foldl (\soFar x -> soFar ++ [f x]) [] list

findClass :: Name -> [Class] -> Class
findClass className theClasses =
  fromMaybe
  (error ("No class with the name " ++ show className ++ " was found!!!"))
  (find ((== className) . #name) theClasses)

findSlotType :: [Class] -> Name -> Type
findSlotType theClasses slotName = let
  theAttributes = concatMap #attributes theClasses :: [Attribute]
  in maybe
    (error "This slot value change is referring to a non-existent attribute!!!")
    #dataType
    (find ((== slotName) . #name) theAttributes)

deduceLvl ::  [Class] -> Class -> Class
deduceLvl _ (Class {classifier = Nothing}) = error "Cannot deduce level of a meta class!!!"
deduceLvl theClasses x@(Class {classifier = Just classifier'}) =
  x <<< (#level (findClass classifier' theClasses) - 1)

deduceValue :: RawSlot -> (Name, Slot)
deduceValue (RawSlot {slotClass, slotName, slotType, rawValue}) =
  if rawValue == "null" then (slotClass, Slot slotName Null) else let
  v = case slotType of
    Boolean Nothing ->  Boolean $ Just $ readCarefully "A Boolean value is invalid!!!" $ toUpper (head rawValue) : tail rawValue
    Integer Nothing -> Integer $ Just $ readCarefully "An Integer value is invalid!!!" rawValue
    Float Nothing -> Float $ Just $ readCarefully "A Float value is invalid!!!" rawValue
    String Nothing -> String $ Just $ map chr $ readCarefully "A String value is invalid!!!" $ head $ splitOn "." rawValue ++ [" "]
    Element Nothing -> Element (Just ())
    MonetaryValue Nothing -> MonetaryValue $ Just (9.99,"unit")
    Date Nothing -> Date $ Just $ readCarefully "A Date value is invalid!!!" $ last $ splitOn "createDate" rawValue
    Currency Nothing -> Currency $ Just EUR
    Complex Nothing -> Complex $ Just rawValue
    AuxiliaryClass Nothing -> AuxiliaryClass $ Just rawValue
    _ -> error "A data type (of Type) cannot be assigned. Only values (of Type) are allowed to do so!!!"
  in
    (slotClass, Slot slotName v)

getBool :: String -> Bool
getBool x = fromMaybe False $ readMaybe $ toUpper (head x) : tail x

getName :: String -> Name
getName = Name . last . splitOn "::"

getMult :: String -> Multiplicity
getMult x = let
  ab = take 2 $ splitOn "," x :: [String]
  a = readCarefully "Invalid lower limit of multiplicity" $ drop 4 $ head ab :: Int
  b = readCarefully "Invalid upper limit of multiplicity" $ last ab :: Int
  in Multiplicity (a, if b == -1 then Nothing else Just b)

getType :: String -> Type
getType x = fromMaybe (Boolean Nothing) $ readMaybe $ (++ " Nothing") $ last $ splitOn "::" x

primt :: Show a => [a] -> IO ()
primt = mapM_ print

fromXModeler :: String -> IO MLM
fromXModeler inputXML = let
  in do
    xml <- readFile inputXML
    let doc = readString [withParseHTML no, withWarnings no] xml
    let extract tag att = runX $ doc //> hasName tag >>> getAttrValue att :: IO [String]
    -------------------
    -- projectName <- getName . head <$> extract "Project" "name" :: IO Name
    let projectName = Name $ "imported_from_" ++ takeWhile (/='.') inputXML
    putStrLn "Got projectName"
    -------------------
    isAbstractMetaClass <- map getBool <$> extract "addMetaClass" "abstract" :: IO [Bool]
    levelMetaClass <- map read <$> extract "addMetaClass" "level" :: IO [Level]
    nameMetaClass <- map Name <$> extract "addMetaClass" "name" :: IO [Name]

    let withMetaClasses = repeat emptyClass |<<<| isAbstractMetaClass |<<<| levelMetaClass |<<<| nameMetaClass
    putStrLn "Got metaclasses"
    -------------------
    isAbstractInstance <- map getBool <$> extract "addInstance" "abstract" :: IO [Bool]
    nameInstance <- map Name <$> extract "addInstance" "name" :: IO [Name]
    classifiers <- map (Just . getName) <$> extract "addInstance" "of" :: IO [Maybe Name]

    let withInstancesButLvlZero = repeat emptyClass |<<<| isAbstractInstance |<<<| nameInstance |<<<| classifiers

    let withInstances = accumulate withMetaClasses withInstancesButLvlZero deduceLvl
    putStrLn "Got instances"
    -------------------
    changeParentClass <- map getName <$> extract "changeParent" "class" :: IO [Name]
    changeParentNew <- map (map getName . splitOn ",") <$> extract "changeParent" "new" :: IO [[Name]]
    let changeParentDict = zip changeParentClass changeParentNew :: [(Name, [Name])]
    let withInheritances = accumulateSimple withInstances (\x -> maybe x (x <<<) (lookup (#name x) changeParentDict) ) :: [Class]
    primt withInheritances
    putStrLn "Got inheritances"
    -------------------
    attributesClasses <- map getName <$> extract "addAttribute" "class" :: IO [Name]
    attributesLevels <- map read <$> extract "addAttribute" "level" :: IO [Int]
    attributesMultiplicities <- map getMult <$> extract "addAttribute" "multiplicity" :: IO [Multiplicity]
    attributesNames <- map Name <$> extract "addAttribute" "name" :: IO [Name]
    attributesTypes <- map getType <$> extract "addAttribute" "type" :: IO [Type]

    let allAttributes = repeat emptyAttribute |<<<| attributesLevels |<<<| attributesMultiplicities |<<<| attributesNames |<<<| attributesTypes

    let attributesDictUngrouped = zip attributesClasses allAttributes :: [(Name, Attribute)]
    -- let attributesDict = concatMap (toList . fromListWith (++)) $ groupBy ((==) `on` fst) attributesDictUngrouped :: [(Name, [Attribute])]
    let attributesDict = toList $ fromListWith (++) $ map (second (:[])) attributesDictUngrouped :: [(Name, [Attribute])]
    let withAttributes = accumulateSimple withInheritances (\x -> maybe x (x <<<) (lookup (#name x) attributesDict)) :: [Class]
    primt attributesDict
    putStrLn "Got attributes"
    -------------------
    opClasses <- map Name <$> extract "addOperation" "class" :: IO [Name]
    opBodies <- map (OperationBody "") <$> extract "changeSlotValue" "valueToBeParsed" :: IO [OperationBody]
    opLvls <- map read <$> extract "addOperation" "level" :: IO [Level]
    opMonitored <- map getBool <$> extract "addOperation" "monitored" :: IO [Bool]
    opNames <- map getName <$> extract "addOperation" "name" :: IO [Name]
    opTypes <- map getType <$> extract "addOperation" "name" :: IO [Type]

    let readyOperations = repeat emptyOperation |<<<| opBodies |<<<| opLvls |<<<| opMonitored |<<<| opNames |<<<| opTypes

    let opDictUngrouped = zip opClasses (map (:[]) readyOperations) :: [(Name, [Operation])]

    let opDict = toList $ fromListWith (++) opDictUngrouped :: [(Name, [Operation])]

    let withOperations = accumulateSimple withAttributes (\x -> maybe x (x <<<) (lookup (#name x) opDict)) :: [Class]
    primt opDict
    putStrLn "Got operations"
    -------------------
    slotClasses <- map getName <$> extract "changeSlotValue" "class" :: IO [Name]
    slotNames <- map getName <$> extract "changeSlotValue" "slotName" :: IO [Name]
    slotValues <- extract "changeSlotValue" "valueToBeParsed" :: IO [String]

    let rawSlotsWithClasses = map (\x -> emptyRawSlot {slotClass = x}) slotClasses :: [RawSlot]
    let rawSlotsWithNames = zipWith (\rawSlot x -> rawSlot {slotName = x}) rawSlotsWithClasses slotNames :: [RawSlot]
    let rawSlotsWithValuesUnfiltered = zipWith (\rawSlot x -> rawSlot {rawValue = x}) rawSlotsWithNames slotValues :: [RawSlot]
    let rawSlotsWithValues = filter (\RawSlot {slotName} -> slotName /= Name "lastUpdated") rawSlotsWithValuesUnfiltered :: [RawSlot]
    let findSlotTypeNow = findSlotType withOperations :: Name -> Type
    let rawSlotsWithTypes = map (\rawSlot@(RawSlot {slotName}) -> rawSlot {slotType = findSlotTypeNow slotName}) rawSlotsWithValues :: [RawSlot]

    let slotsDictUngrouped = map (second (:[]) . deduceValue) rawSlotsWithTypes :: [(Name, [Slot])]

    let slotsDict = toList $ fromListWith (++) slotsDictUngrouped :: [(Name, [Slot])]

    let withSlots = accumulateSimple withOperations (\x -> maybe x (x <<<) (lookup (#name x) slotsDict)) :: [Class]
    primt slotsDict
    putStrLn "Got slots"
    -------------------
    assoSources <- map getName <$> extract "addAssociation" "classSource" :: IO [Name]
    assoTargets <- map getName <$> extract "addAssociation" "classTarget" :: IO [Name]
    assoNames <- map Name <$> extract "addAssociation" "fwName" :: IO [Name]
    assoSourceLvls <- map read <$> extract "addAssociation" "instLevelSource" :: IO [Level]
    assoTargetLvls <- map read <$> extract "addAssociation" "instLevelTarget" :: IO [Level]
    assoMultSourceToTarget <- map getMult <$> extract "addAssociation" "multSourceToTarget" :: IO [Multiplicity]
    assoMultTargetToSource <- map getMult <$> extract "addAssociation" "multTargetToSource" :: IO [Multiplicity]
    assoVisibilitySource <- map getBool <$> extract "addAssociation" "sourceVisibleFromTarget" :: IO [Bool]
    assoVisibilityTarget <- map getBool <$> extract "addAssociation" "targetVisibleFromSource" :: IO [Bool]

    let readyAssociations = repeat emptyAssociation |<<<| zip (repeat Source) assoSources |<<<| zip (repeat Target) assoTargets |<<<| assoNames |<<<| zip (repeat Source) assoSourceLvls |<<<| zip (repeat Target) assoTargetLvls |<<<| zip (repeat Source) assoMultSourceToTarget |<<<| zip (repeat Target) assoMultTargetToSource |<<<| zip (repeat Source) assoVisibilitySource |<<<| zip (repeat Target) assoVisibilityTarget
    primt readyAssociations
    print "Got associations"
    -------------------
    linkNames <- map getName <$> extract "addLink" "name" :: IO [Name]
    linkSources <- map getName <$> extract "addLink" "classSource" :: IO [Name]
    linkTargets <- map getName <$> extract "addLink" "classTarget" :: IO [Name]

    let readyLinks = repeat emptyLink |<<<| linkNames |<<<| zip (repeat Source) linkSources |<<<| zip (repeat Target) linkTargets
    primt readyLinks
    putStrLn "Got links"
    -------------------

    let readyClasses = withSlots
    return $ MLM projectName readyClasses readyAssociations readyLinks