module Modelling.MLM.Modify (
    (<<<),
    (|<<<|),
    SourceOrTarget(..),
    insert
    ) where

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
  Level,
  Type (..),
  Value (..)
  )

class Modifiable a b where
    (<<<) :: a -> b -> a
class CanBeInsertedInto a i b where
    insert :: a -> i -> b -> a

instance CanBeInsertedInto MLM Name Attribute where
    insert mlm@MLM{classes} classToInsertInto attribute =
        mlm{classes = map (\class' -> if #name class' == classToInsertInto then class' <<< attribute else class') classes}
instance CanBeInsertedInto MLM Name Slot where
    insert mlm@MLM{classes} classToInsertInto slot =
        mlm{classes = map (\class' -> if #name class' == classToInsertInto then class' <<< slot else class') classes}
instance CanBeInsertedInto MLM Name Operation where
    insert mlm@MLM{classes} classToInsertInto operation =
        mlm{classes = map (\class' -> if #name class' == classToInsertInto then class' <<< operation else class') classes}

(|<<<|) :: Modifiable a b => [a] -> [b] -> [a]
(|<<<|) = zipWith (<<<)

data SourceOrTarget = Source | Target

-------- MLM
instance Modifiable MLM Name where
    (<<<) y x = y{name = x}
instance Modifiable MLM Class where
    (<<<) y@MLM{classes} x = y{classes = classes ++ [x]}
instance Modifiable MLM Association where
    (<<<) y@MLM{associations} x = y{associations = associations ++ [x]}
instance Modifiable MLM Link where
    (<<<) y@MLM{links} x = y{links = links ++ [x]}

-------- Class
instance Modifiable Class Name where
    (<<<) y x = y{name = x}
instance Modifiable Class Bool where
    (<<<) y x = y{isAbstract = x}
instance Modifiable Class Level where
    (<<<) y x = y{level = x}
instance Modifiable Class [Name] where
    (<<<) y x = y{parents = x}
instance Modifiable Class (Maybe Name) where
    (<<<) y x = y{classifier = x}
instance Modifiable Class Attribute where
    (<<<) y@Class{attributes} x = y{attributes = attributes ++ [x]}
instance Modifiable Class [Attribute] where
    (<<<) y@Class{attributes} x = y{attributes = attributes ++ x}
instance Modifiable Class Operation where
    (<<<) y@Class{operations} x = y{operations = operations ++ [x]}
instance Modifiable Class [Operation] where
    (<<<) y@Class{operations} x = y{operations = operations ++ x}
instance Modifiable Class Slot where
    (<<<) y@Class{slots} x = y{slots = slots ++ [x]}
instance Modifiable Class [Slot] where
    (<<<) y@Class{slots} x = y{slots = slots ++ x}

-------- Attribute
instance Modifiable Attribute Level where
    (<<<) y x = y{level = x}
instance Modifiable Attribute Name where
    (<<<) y x = y{name = x}
instance Modifiable Attribute Type where
    (<<<) y x = y{dataType = x}
instance Modifiable Attribute Multiplicity where
    (<<<) y x = y{multiplicity = x}

-------- Slot
instance Modifiable Slot Name where
    (<<<) y x = y{name = x}
instance Modifiable Slot Value where
    (<<<) y x = y{value = x}

-------- Operation
instance Modifiable Operation Int where
    (<<<) y x = y{level = x}
instance Modifiable Operation Name where
    (<<<) y x = y{name = x}
instance Modifiable Operation Type where
    (<<<) y x = y{dataType = x}
instance Modifiable Operation Bool where
    (<<<) y x = y{isMonitored = x}
instance Modifiable Operation String where
    (<<<) y x = y{body = x}

-------- Association
instance Modifiable Association Name where
    (<<<) y x = y{name = x}
instance Modifiable Association (SourceOrTarget, Name) where
    (<<<) y (direction, x) = case direction of
            Source -> y{source = x}
            Target -> y{target = x}
instance Modifiable Association (SourceOrTarget, Level) where
    (<<<) y (direction, x) = case direction of
            Source -> y{lvlSource = x}
            Target -> y{lvlTarget = x}
instance Modifiable Association (SourceOrTarget, Multiplicity) where
    (<<<) y (direction, x) = case direction of
            Source -> y{multSource = x}
            Target -> y{multTarget = x}
instance Modifiable Association (SourceOrTarget, Bool) where
    (<<<) y (direction, x) = case direction of
            Source -> y{visibleSource = x}
            Target -> y{visibleTarget = x}

-------- Link
instance Modifiable Link Name where
    (<<<) y x = y{name = x}
instance Modifiable Link (SourceOrTarget, Name) where
    (<<<) y (direction, x) = case direction of
            Source -> y{source = x}
            Target -> y{target = x}

