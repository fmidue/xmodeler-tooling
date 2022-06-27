{-# LANGUAGE QuasiQuotes #-}

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