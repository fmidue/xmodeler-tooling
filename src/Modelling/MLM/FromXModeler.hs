module Modelling.MLM.FromXModeler (fromXModeler) where

import Modelling.MLM.Types (
  MLM (..),
  Name (..),
  Level,
  Class (..),
  Multiplicity (..),
  Type (..),
  Value (..),
  Attribute (..),
  Slot (..),
  Operation (..),
  emptyClass,
  emptyAttribute,
  emptyAssociation,
  emptyLink,
  emptyName,
  emptyOperation,
  generateClassFinder,
  generateInstantiatableAttributesFinder
  )

import Text.XML.HXT.Core (readString, runX, hasName, getAttrValue, withParseHTML, withWarnings, no, (//>), (>>>))
import Modelling.MLM.Modify ((<<<),(|<<<|), SourceOrTarget (..))
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Map (Map, fromListWith, fromList, (!?))
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (second)
import Data.Char (toUpper, chr)
import Text.Read (readMaybe)

data RawSlot = RawSlot {
  slotName :: Name,
  slotType :: Type,
  rawValue :: String,
  slotClass :: Name
} deriving Show

readCarefully :: Read a => String -> (String -> a)
readCarefully x = fromMaybe (error x) . readMaybe

emptyRawSlot :: RawSlot
emptyRawSlot = RawSlot emptyName Boolean "" emptyName

accumulate :: [a] -> [b] -> ([a] -> b -> a) -> [a]
accumulate start list f = foldl (\soFar x -> soFar ++ [f soFar x]) start list

generateSlotTypeFinder :: [Class] -> (RawSlot -> RawSlot)
generateSlotTypeFinder theClasses rawSlot@RawSlot{slotClass, slotName} = let

  findClassMaybe :: Name -> Maybe Class
  findClassMaybe = generateClassFinder theClasses

  instantiatableAttributes :: Class -> [Attribute]
  instantiatableAttributes = generateInstantiatableAttributesFinder theClasses

  instantiatableAttributesMaybe :: Name -> [Attribute]
  instantiatableAttributesMaybe = maybe [] instantiatableAttributes . findClassMaybe

  typeFound :: Type
  typeFound = maybe
    (error "This slot value change is referring to a non-existent attribute!!!")
    #dataType
    (find ((== slotName) . #name) (instantiatableAttributesMaybe slotClass))
  in rawSlot {slotType = typeFound}

findClass :: Name -> [Class] -> Class
findClass className theClasses =
  fromMaybe
  (error ("No class with the name " ++ show className ++ " was found!!!"))
  (find ((== className) . #name) theClasses)

deduceLvl ::  [Class] -> Class -> Class
deduceLvl _ Class{classifier = Nothing} = error "Cannot deduce level of a meta class!!!"
deduceLvl theClasses x@Class{classifier = Just classifier'} =
  x <<< (#level (findClass classifier' theClasses) - 1)

deduceValue :: RawSlot -> (Name, Slot)
deduceValue RawSlot{slotClass, slotName, slotType, rawValue} = let

  extractCurrency :: String -> String
  extractCurrency [] = error "A Currency value is invalid!!!"
  extractCurrency x = let
    y = splitOn "\"" x in
    if null y then "" else y !! 3

  extractMonetaryValue :: String -> (String, String)
  extractMonetaryValue [] = error "A MonetaryValue is invalid!!!"
  extractMonetaryValue x = let
    y = splitOn "Auxiliary::MonetaryValue(" x :: [String]
    z = if length y < 2 then [] else splitOn ", Auxiliary::Currency" (y !! 1)
    in if null z then ([],[]) else (head z, extractCurrency x)

  v = case slotType of
    Boolean ->  VBoolean $ readCarefully "A Boolean value is invalid!!!" $ toUpper (head rawValue) : tail rawValue
    Integer -> VInteger $ readCarefully "An Integer value is invalid!!!" rawValue
    Float -> VFloat $ readCarefully "A Float value is invalid!!!" rawValue
    String -> VString $ map chr $ readCarefully "A String value is invalid!!!" $ head $ splitOn "." rawValue ++ [" "]
    Element -> VElement rawValue
    MonetaryValue -> VMonetaryValue $ extractMonetaryValue rawValue
    Date -> VDate $ readCarefully "A Date value is invalid!!!" $ last $ splitOn "createDate" rawValue
    Currency -> VCurrency $ readCarefully "A Currency value is invalid!!!" $ extractCurrency rawValue
    Complex -> VComplex rawValue
    AuxiliaryClass -> VAuxiliaryClass rawValue
  in
    (slotClass, Slot slotName v)

getBool :: String -> Bool
getBool x = Just True == readMaybe (toUpper (head x) : tail x)

getName :: String -> Name
getName = Name . last . splitOn "::"

getMult :: String -> Multiplicity
getMult x = let
  ab = take 2 $ splitOn "," x :: [String]
  a = readCarefully "Invalid lower limit of multiplicity" $ drop 4 $ head ab :: Int
  b = readCarefully "Invalid upper limit of multiplicity" $ last ab :: Int
  in Multiplicity (a, if b == -1 then Nothing else Just b)

getType :: String -> Type
getType x = fromMaybe Boolean $ readMaybe $ last $ splitOn "::" x

fromXModeler :: String -> IO MLM
fromXModeler inputXML = let
  in do
    xml <- readFile inputXML
    let doc = readString [withParseHTML no, withWarnings no] xml
    let extract tag att = runX $ doc //> hasName tag >>> getAttrValue att :: IO [String]
    -----------------------------------------------------------------------------------------------
    let projectName = Name $ "imported_from_" ++ takeWhile (/= '.') (last $ splitOn "/" inputXML)
    -----------------------------------------------------------------------------------------------
    isAbstractMetaClass <- map getBool <$> extract "addMetaClass" "abstract" :: IO [Bool]
    levelMetaClass <- map read <$> extract "addMetaClass" "level" :: IO [Level]
    nameMetaClass <- map Name <$> extract "addMetaClass" "name" :: IO [Name]

    let withMetaClasses = repeat emptyClass |<<<| isAbstractMetaClass |<<<| levelMetaClass |<<<| nameMetaClass
    -----------------------------------------------------------------------------------------------
    isAbstractInstance <- map getBool <$> extract "addInstance" "abstract" :: IO [Bool]
    nameInstance <- map Name <$> extract "addInstance" "name" :: IO [Name]
    classifiers <- map (Just . getName) <$> extract "addInstance" "of" :: IO [Maybe Name]

    let withInstancesButLvlZero = repeat emptyClass |<<<| isAbstractInstance |<<<| nameInstance |<<<| classifiers

    let withInstances = accumulate withMetaClasses withInstancesButLvlZero deduceLvl
    -----------------------------------------------------------------------------------------------
    changeParentClass <- map getName <$> extract "changeParent" "class" :: IO [Name]
    changeParentNew <- map (map getName . splitOn ",") <$> extract "changeParent" "new" :: IO [[Name]]
    let changeParentDict = fromList $ zip changeParentClass changeParentNew :: Map Name [Name]
    let withInheritances = map (\x -> maybe x (x <<<) (changeParentDict !? #name x)) withInstances :: [Class]
    -----------------------------------------------------------------------------------------------
    attributeClasses <- map getName <$> extract "addAttribute" "class" :: IO [Name]
    attributeLevels <- map read <$> extract "addAttribute" "level" :: IO [Int]
    attributeMultiplicities <- map getMult <$> extract "addAttribute" "multiplicity" :: IO [Multiplicity]
    attributeNames <- map Name <$> extract "addAttribute" "name" :: IO [Name]
    attributeTypes <- map getType <$> extract "addAttribute" "type" :: IO [Type]

    let allAttributes = repeat emptyAttribute |<<<| attributeLevels |<<<| attributeMultiplicities |<<<| attributeNames |<<<| attributeTypes

    let attributeDictUngrouped = zip attributeClasses (map (:[]) allAttributes) :: [(Name, [Attribute])]
    let attributeDict = fromListWith (++) attributeDictUngrouped :: Map Name [Attribute]
    let withAttributes = map (\x -> maybe x (x <<<) (attributeDict !? #name x)) withInheritances :: [Class]
    -----------------------------------------------------------------------------------------------
    operationClasses <- map getName <$> extract "addOperation" "class" :: IO [Name]
    operationBodies <- extract "addOperation" "body" :: IO [String]
    operationLevels <- map read <$> extract "addOperation" "level" :: IO [Level]
    operationMonitored <- map getBool <$> extract "addOperation" "monitored" :: IO [Bool]
    operationNames <- map Name <$> extract "addOperation" "name" :: IO [Name]
    operationTypes <- map getType <$> extract "addOperation" "type" :: IO [Type]

    let readyOperations = repeat emptyOperation |<<<| operationBodies |<<<| operationLevels |<<<| operationMonitored |<<<| operationNames |<<<| operationTypes :: [Operation]

    let operationDictUngrouped = zip operationClasses (map (:[]) readyOperations) :: [(Name, [Operation])]

    let operationDict = fromListWith (++) operationDictUngrouped :: Map Name [Operation]

    let withOperations = map (\x -> maybe x (x <<<) (operationDict !? #name x)) withAttributes :: [Class]
    -----------------------------------------------------------------------------------------------
    slotClasses <- map getName <$> extract "changeSlotValue" "class" :: IO [Name]
    slotNames <- map getName <$> extract "changeSlotValue" "slotName" :: IO [Name]
    slotValues <- extract "changeSlotValue" "valueToBeParsed" :: IO [String]

    let rawSlotsWithClasses = map (\x -> emptyRawSlot {slotClass = x}) slotClasses :: [RawSlot]
    let rawSlotsWithNames = zipWith (\rawSlot x -> rawSlot {slotName = x}) rawSlotsWithClasses slotNames :: [RawSlot]
    let rawSlotsWithValuesUnfiltered = zipWith (\rawSlot x -> rawSlot {rawValue = x}) rawSlotsWithNames slotValues :: [RawSlot]
    let rawSlotsWithValues = filter (\RawSlot {slotName} -> slotName /= Name "lastUpdated") rawSlotsWithValuesUnfiltered :: [RawSlot]
    let findSlotType = generateSlotTypeFinder withOperations :: RawSlot -> RawSlot
    let rawSlotsWithTypes = map findSlotType rawSlotsWithValues :: [RawSlot]
    let slotsDictUngrouped = map (second (:[]) . deduceValue) rawSlotsWithTypes :: [(Name, [Slot])]
    let slotsDict = fromListWith (++) slotsDictUngrouped :: Map Name [Slot]

    let withSlots = map (\x -> maybe x (x <<<) (slotsDict !? #name x)) withOperations :: [Class]
    -----------------------------------------------------------------------------------------------
    associationSources <- map getName <$> extract "addAssociation" "classSource" :: IO [Name]
    associationTargets <- map getName <$> extract "addAssociation" "classTarget" :: IO [Name]
    associationNames <- map Name <$> extract "addAssociation" "fwName" :: IO [Name]
    associationSourceLevels <- map read <$> extract "addAssociation" "instLevelSource" :: IO [Level]
    associationTargetLevels <- map read <$> extract "addAssociation" "instLevelTarget" :: IO [Level]
    associationMultTarget <- map getMult <$> extract "addAssociation" "multSourceToTarget" :: IO [Multiplicity]
    associationMultTargetToSource <- map getMult <$> extract "addAssociation" "multTargetToSource" :: IO [Multiplicity]
    associationVisibilitySource <- map getBool <$> extract "addAssociation" "sourceVisibleFromTarget" :: IO [Bool]
    associationVisibilityTarget <- map getBool <$> extract "addAssociation" "targetVisibleFromSource" :: IO [Bool]

    let readyAssociations = repeat emptyAssociation |<<<| zip (repeat Source) associationSources |<<<| zip (repeat Target) associationTargets |<<<| associationNames |<<<| zip (repeat Source) associationSourceLevels |<<<| zip (repeat Target) associationTargetLevels |<<<| zip (repeat Target) associationMultTarget |<<<| zip (repeat Source) associationMultTargetToSource |<<<| zip (repeat Source) associationVisibilitySource |<<<| zip (repeat Target) associationVisibilityTarget
    -----------------------------------------------------------------------------------------------
    linkNames <- map getName <$> extract "addLink" "name" :: IO [Name]
    linkSources <- map getName <$> extract "addLink" "classSource" :: IO [Name]
    linkTargets <- map getName <$> extract "addLink" "classTarget" :: IO [Name]

    let readyLinks = repeat emptyLink |<<<| linkNames |<<<| zip (repeat Source) linkSources |<<<| zip (repeat Target) linkTargets
    -----------------------------------------------------------------------------------------------
    let readyClasses = withSlots
    return $ MLM projectName readyClasses readyAssociations readyLinks