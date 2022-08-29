module Modelling.MLM.Modify ((<<<), (|<<<|), SourceOrTarget(..)) where

import Modelling.MLM.Types (
  MLM (..),
  Link (..),
  Association (..),
  Slot (..),
  Class (..),
  Operation (..),
  Attribute (..),
  Multiplicity (..),
  Name (..),
  OperationBody (..),
  Level,
  Type (..),
  Value (..)
  )

class Modifiable a b where
    (<<<) :: a -> b -> a

(|<<<|) :: Modifiable a b => [a] -> [b] -> [a]
(|<<<|) = zipWith (<<<)

data SourceOrTarget = Source | Target

-------- MLM
instance Modifiable MLM Name where
    (<<<) y x = y {name = x}
instance Modifiable MLM Class where
    (<<<) y@(MLM {classes}) x = y {classes = classes ++ [x]}
instance Modifiable MLM Association where
    (<<<) y@(MLM {associations}) x = y {associations = associations ++ [x]}
instance Modifiable MLM Link where
    (<<<) y@(MLM {links}) x = y {links = links ++ [x]}

-------- Class
instance Modifiable Class Name where
    (<<<) y x = y {name = x}
instance Modifiable Class Bool where
    (<<<) y x = y {isAbstract = x}
instance Modifiable Class Level where
    (<<<) y x = y {level = x}
instance Modifiable Class [Name] where
    (<<<) y x = y {parents = x}
instance Modifiable Class (Maybe Name) where
    (<<<) y x = y {classifier = x}
instance Modifiable Class Attribute where
    (<<<) y@(Class {attributes}) x = y {attributes = attributes ++ [x]}
instance Modifiable Class [Attribute] where
    (<<<) y@(Class {attributes}) x = y {attributes = attributes ++ x}
instance Modifiable Class [Operation] where
    (<<<) y x = y {operations = x}
instance Modifiable Class Slot where
    (<<<) y@(Class {slots}) x = y {slots = slots ++ [x]}
instance Modifiable Class [Slot] where
    (<<<) y@(Class {slots}) x = y {slots = slots ++ x}

-------- Attribute
instance Modifiable Attribute Level where
    (<<<) y x = y {level = x}
instance Modifiable Attribute Name where
    (<<<) y x = y {name = x}
instance Modifiable Attribute Type where
    (<<<) y x = y {dataType = x}
instance Modifiable Attribute Multiplicity where
    (<<<) y x = y {multiplicity = x}

-------- Slot
instance Modifiable Slot Name where
    (<<<) y x = y {name = x}
instance Modifiable Slot Value where
    (<<<) y x = y {value = x}

-------- Operation
instance Modifiable Operation Int where
    (<<<) y x = y {level = x}
instance Modifiable Operation Name where
    (<<<) y x = y {name = x}
instance Modifiable Operation Type where
    (<<<) y x = y {dataType = x}
instance Modifiable Operation Bool where
    (<<<) y x = y {isMonitored = x}
instance Modifiable Operation OperationBody where
    (<<<) y x = y {body = x}

-------- Association
instance Modifiable Association Name where
    (<<<) y x = y {name = x}
instance Modifiable Association (SourceOrTarget, Name) where
    (<<<) y (direction, x) = case direction of
            Source -> y {source = x}
            Target -> y {target = x}
instance Modifiable Association (SourceOrTarget, Level) where
    (<<<) y (direction, x) = case direction of
            Source -> y {lvlSource = x}
            Target -> y {lvlTarget = x}
instance Modifiable Association (SourceOrTarget, Multiplicity) where
    (<<<) y (direction, x) = case direction of
            Source -> y {multTargetToSource = x}
            Target -> y {multSourceToTarget = x}
instance Modifiable Association (SourceOrTarget, Bool) where
    (<<<) y (direction, x) = case direction of
            Source -> y {sourceVisibleFromTarget = x}
            Target -> y {targetVisibleFromSource = x}

-------- Link
instance Modifiable Link Name where
    (<<<) y x = y {name = x}
instance Modifiable Link (SourceOrTarget, Name) where
    (<<<) y (direction, x) = case direction of
            Source -> y {source = x}
            Target -> y {target = x}

-- class V a b where
--     (<?<) :: a -> b -> Maybe a

-- instance V MLM (Name, Level, Bool, [Name]) where
--     (<?<) mlm@(MLM {classes, associations, links}) (n, l, a, ps) = let
--         in
--             if all []
--                 then
--                     Just (mlm <<< Class a l n ps [] [] [])
--                 else
--                     Nothing
