module Modelling.MLM.Edit (
    Edit,
    edit,
    performRandomEdit,
    performRandomEdits,
    performRandomEditsAndReturnLastValidOne,
    performRandomEditsAndReturnLastValidOneButDropLastEditIfInvalidInsteadOfBuildingOnTopOfITPlease
    ) where

import Modelling.MLM.Types (
--   Validatable (..),
  MLM (..),
  Link (..),
  Association (..),
--   Slot (..),
  Class (..),
  Operation (..),
  Attribute (..),
  Multiplicity (..),
  Name (..),
  Level,
  Type (..),
--   Value (..),
  valid,
  typeSpace
  )
import Modelling.MLM.Modify ((<<<))
import Modelling.MLM.GenerateMLM (
    classNameSpace,
    associationNameSpace,
    attributeNameSpace,
    -- randomSlotValue,
    operationNameSpace
    )

import Test.QuickCheck (Gen, elements, chooseAny, chooseInt, oneof, sublistOf, frequency)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)

performRandomEdit :: Bool -> MLM -> Gen MLM
performRandomEdit shouldWeForbidDeleteComponents mlm = edit mlm =<< randomEdit shouldWeForbidDeleteComponents

performRandomEdits :: Bool -> Int -> MLM -> Gen MLM
performRandomEdits shouldWeForbidDeleteComponents n mlm = last <$> replicateM n (performRandomEdit shouldWeForbidDeleteComponents mlm)

performRandomEditsAndReturnLastValidOne :: Bool -> Int -> MLM -> Gen MLM
performRandomEditsAndReturnLastValidOne shouldWeForbidDeleteComponents n mlm = last . filter (valid ()) <$> replicateM n (performRandomEdit shouldWeForbidDeleteComponents mlm)

performRandomEditsAndReturnLastValidOneButDropLastEditIfInvalidInsteadOfBuildingOnTopOfITPlease :: Bool -> Int -> MLM -> Gen MLM
performRandomEditsAndReturnLastValidOneButDropLastEditIfInvalidInsteadOfBuildingOnTopOfITPlease shouldWeForbidDeleteComponents n mlm
    | n < 1 = return mlm
    | otherwise = do
        x <- performRandomEdit shouldWeForbidDeleteComponents mlm
        let y = if valid () x then x else mlm
        performRandomEditsAndReturnLastValidOneButDropLastEditIfInvalidInsteadOfBuildingOnTopOfITPlease shouldWeForbidDeleteComponents (n-1) y

generateNextAvailableClassNameFinder :: MLM -> Name
generateNextAvailableClassNameFinder MLM{classes} = fromMaybe (error "Impossible to not find an available class name in the infinite list of names!!!") $
    find (`notElem` map #name classes) classNameSpace

generateNextAvailableAssociationNameFinder :: MLM -> Name
generateNextAvailableAssociationNameFinder MLM{associations} = fromMaybe (error "Impossible to not find an available association name in the infinite list of names!!!") $
    find (`notElem` map #name associations) associationNameSpace

generateNextAvailableAttributeNameFinder :: MLM -> Name
generateNextAvailableAttributeNameFinder MLM{classes} = fromMaybe (error "Impossible to not find an available attribute name in the infinite list of names!!!") $
    find (`notElem` map #name (concatMap #attributes classes)) attributeNameSpace

generateNextAvailableOperationNameFinder :: MLM -> Name
generateNextAvailableOperationNameFinder MLM{classes} = fromMaybe (error "Impossible to not find an available operation name in the infinite list of names!!!") $
    find (`notElem` map #name (concatMap #operations classes)) operationNameSpace

data Edit = AddClass | AddAssociation | AddLink | AddAttribute | AddOperation | DeleteClass | DeleteAssociation | DeleteLink | DeleteAttribute | DeleteOperation deriving (Bounded, Enum)

randomEdit :: Bool -> Gen Edit
randomEdit shouldWeForbidDeleteComponents = if shouldWeForbidDeleteComponents
    then elements [AddClass, AddAssociation, AddLink]--, AddAttribute, AddOperation]
    else elements [minBound .. maxBound]

edit :: MLM -> Edit -> Gen MLM
edit mlm@MLM{classes, associations, links} e = let

    classesNames = map #name classes :: [Name]
    associationsNames = map #name associations :: [Name]
    attributesNames = map #name $ concatMap #attributes classes :: [Name]
    operationsNames = map #name $ concatMap #operations classes :: [Name]
    nextAvailableClassName :: Name
    nextAvailableClassName = generateNextAvailableClassNameFinder mlm

    nextAvailableAssociationName :: Name
    nextAvailableAssociationName = generateNextAvailableAssociationNameFinder mlm

    nextAvailableAttributeName :: Name
    nextAvailableAttributeName = generateNextAvailableAttributeNameFinder mlm

    nextAvailableOperationName :: Name
    nextAvailableOperationName = generateNextAvailableOperationNameFinder mlm

    randomBool :: Gen Bool
    randomBool = chooseAny

    randomMult :: Gen Multiplicity
    randomMult = do
        a <- chooseInt (0, maxBound)
        b <- oneof [return Nothing, Just <$> chooseInt (a, maxBound)]
        return $ Multiplicity (a, b)

    randomLevel :: Gen Level
    randomLevel = chooseInt (0, maxBound)

    randomType :: Gen Type
    randomType = elements typeSpace

    exceptOnesNamed list x = filter ((/= x) . #name) list

    in case e of
        AddClass -> do
            isAbstract' <- randomBool
            level' <- randomLevel
            let name' = nextAvailableClassName
            parents' <- sublistOf classesNames
            classifier' <- frequency [
                    (1, return Nothing),
                    (1, Just <$> elements classesNames)
                ]
            let attributes' = []
            let operations' = []
            let slots' = []
            return $ mlm <<< Class isAbstract' level' name' parents' classifier' attributes' operations' slots'
        AddAssociation -> do
            let name' = nextAvailableAssociationName
            source' <- elements classesNames
            target' <- elements classesNames
            lvlSource' <- randomLevel
            lvlTarget' <- randomLevel
            multSource' <- randomMult
            multTarget' <- randomMult
            visibleSource' <- chooseAny :: Gen Bool
            visibleTarget' <- chooseAny :: Gen Bool
            return $ mlm <<< Association name' source' target' lvlSource' lvlTarget' multSource' multTarget' visibleSource' visibleTarget'
        AddLink -> do
            name' <- elements associationsNames
            source' <- elements classesNames
            target' <- elements classesNames
            return $ mlm <<< Link name' source' target'
        AddAttribute -> do
            level' <- randomLevel
            let name' = nextAvailableAttributeName
            dataType' <- randomType
            multiplicity' <- randomMult
            randomClass <- elements classesNames
            let attribute = Attribute level' name' dataType' multiplicity'
            return $ mlm{classes = map (\class'@Class{name} ->
                    if name == randomClass then class' <<< attribute else class'
                ) classes}
        AddOperation -> do
            level' <- randomLevel
            let name' = nextAvailableOperationName
            dataType' <- randomType
            isMonitored' <- randomBool
            let body' = ""
            randomClass <- elements classesNames
            let operation = Operation level' name' dataType' isMonitored' body'
            return $ mlm{classes = map (\class'@Class{name} ->
                if name == randomClass then class' <<< operation else class') classes}
        -- AddSlot -> do
        --     name' <- elements attributesNames
        --     value' <- maybe
        --         (error "I just took the name of the attribute that I am searching for from the list of attributes names itself, but now I cannot find it!!!")
        --         randomSlotValue
        --         (find
        --                 ((== name') . #name)
        --                 (concatMap #attributes classes)
        --             )
        --     return $ mlm{classes = }
        DeleteClass -> do
            randomClass <- elements classesNames
            return $ mlm{classes = classes `exceptOnesNamed` randomClass}
        DeleteAssociation -> do
            randomAssociation <- elements associationsNames
            return $ mlm{associations = associations `exceptOnesNamed` randomAssociation}
        DeleteLink -> do
            randomLink <- elements $ map #name links
            return $ mlm{links = links `exceptOnesNamed` randomLink}
        DeleteAttribute -> do
            randomAttribute <- elements attributesNames
            return $ mlm{classes = map (\class'@Class{attributes} -> class'{attributes = attributes `exceptOnesNamed` randomAttribute}) classes}
        DeleteOperation -> do
            randomOperation <- elements operationsNames
            return $ mlm{classes = map (\class'@Class{operations} -> class'{operations = operations `exceptOnesNamed` randomOperation}) classes}

-- data Edit = AddClass | AddAssociation | AddLink | AddAttribute | AddOperation | AddSlot | DeleteClass | DeleteAssociation | DeleteLink | DeleteAttribute | DeleteOperation | DeleteSlot | ChangeTypeAttribute | ChangeTypeOperation

-- try0 :: MLM -> info -> MLM
-- try0 mlm x = let mlm' = mlm <<< x in
--     if valid () mlm' then mlm' else mlm

-- edit :: MLM -> Edit -> Maybe info -> Gen MLM
-- edit mlm@MLM{classes, associations, links} editType (Just x) = let
--     try = try0 mlm

--     exceptOnesNamed x = filter ((/= x) . #name)

--     in case editType of
--         AddClass -> return $ try x
--         AddAssociation -> edit mlm AddClass (Just x)
--         AddLink -> edit mlm AddClass (Just x)
--         AddAttribute -> let
--             classes' = map (<<< x) classes
--             mlms = map (mlm <<<) classes'
--             validOnes = filter (valid ()) mlms
--             in if null validOnes then return mlm else
--                 elements validOnes
--         AddOperation -> edit mlm AddAttribute (Just x)
--         AddSlot -> edit mlm AddAttribute (Just x)
--         DeleteClass -> return mlm{classes = classes `exceptOnesNamed` x}
--         DeleteAssociation -> return mlm{associations = associations `exceptOnesNamed` x}
--         DeleteLink -> return mlm{links = links `exceptOnesNamed` x}
--         DeleteAttribute -> return mlm{classes = map (\class' -> class'{attributes = attributes `exceptOnesNamed` x}) classes}
--         DeleteOperation -> return mlm{classes = map (\class' -> class'{operations = operations `exceptOnesNamed` x}) classes}
--         DeleteSlot -> return mlm{classes = map (\class' -> class'{slots = slots `exceptOnesNamed` x}) classes}
--         ChangeTypeAttribute -> do
--             randomType <- elements typeSpace
--             return mlm{classes = map (\class' -> class'{attributes = map (\attribute@Attribute{name} ->
--                     if name == x then attribute{dataType = randomType} else attribute
--                 ) attributes}) classes}
--         ChangeTypeOperation -> do
--             randomType <- elements typeSpace
--             return mlm{classes = map (\class' -> class'{operations = map (\operation@Operation{name} ->
--                     if name == x then operation{dataType = randomType} else operation
--                 )})}