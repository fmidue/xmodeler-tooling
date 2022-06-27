{-# LANGUAGE QuasiQuotes #-}

getValue :: Type -> String
getValue t = case t of
  Auxiliary_MonetaryValue (Just (unit, value)) -> [i|Auxiliary::MonetaryValue(#{value}, Auxiliary::Currency(&quot;#{unit}&quot;, &quot;#{value}&quot;, 1.0))|]
  Auxiliary_Date (Just (year, month, day)) -> [i|Auxiliary::Date::createDate(#{year}, #{month}, #{day})|]
  Auxiliary_Currency (Just x) -> [i|Auxiliary::Currency(&quot;$&quot;, &quot;#{map toLower (show x)}&quot;, #{relativeToEur x})|]
  Auxiliary_Complex (Just _) -> "null"
  Auxiliary_AuxiliaryClass (Just _) -> "null"
  XCore_Boolean (Just x) -> map toLower (show x)
  XCore_Integer (Just x) -> show x
  XCore_Float (Just x) -> show x
  XCore_String (Just x) -> [i|#{map ord x}.asString()|]
  XCore_Element (Just _) -> "null"
  _ -> error "Invalid Type and/or Value!"

data MetaClass = MetaClass {
  mAbstract :: Bool,
  mLevel :: Int,
  mName :: String
  }

data Instance = Instance {
  iAbstract :: Bool,
  iName :: String,
  iOf :: String
  }

data ChangeParent = ChangeParent {
  pClass :: String,
  pNew :: [String]
  }

data Attribute = Attribute {
  aClass :: String,
  aLevel :: Int,
  aName :: String,
  aType :: Type,
  aMultiplicityMin :: Int,
  aMultiplicityMax :: Int
  }

data Operation = Operation {
  oClass :: String,
  oLevel :: Int,
  oName :: String,
  oType :: Type,
  oBody :: String
  }

data ChangeSlotValue = ChangeSlotValue {
  vClass :: String,
  vName :: String,
  vValueToBeParsed :: Type
  }

data Association = Association {
  sFwName :: String,
  sSource :: String,
  sTarget :: String,
  sInstLevelSource :: Int,
  sInstLevelTarget :: Int,
  sMultTargetToSourceMin :: Int,
  sMultTargetToSourceMax :: Int,
  sMultSourceToTargetMin :: Int,
  sMultSourceToTargetMax :: Int,
  sSourceVisibleFromTarget :: Bool,
  sTargetVisibleFromSource :: Bool
  }

data Link = Link {
  lName :: String,
  lClassSource :: String,
  lClassTarget :: String
  }

data MLM = MLM {
  mlmName :: String,
  mlmMetaClasses :: [MetaClass],
  mlmInstances :: [Instance],
  mlmChangeParents :: [ChangeParent],
  mlmAttributes :: [Attribute],
  mlmOperations :: [Operation],
  mlmChangeSlotValues :: [ChangeSlotValue],
  mlmAssociations :: [Association],
  mlmLinks :: [Link]
  }