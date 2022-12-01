-- 1.
-- data MyInt = MkMyInt Int
-- 6.
newtype MyInt = MkMyInt Int

-- 2.
instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
-- 3.
instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2
-- 4.
instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i)            = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (abs i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromIntegral int)
-- 5.
instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i


data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)


instance Eq a => Eq (BinTree a) where
    (==) (EmptyBT) (EmptyBT)                 = True
    (==) (NodeBT a l r) (EmptyBT)            = False
    (==) (EmptyBT) (NodeBT a l r)            = False
    (==) (NodeBT a1 l1 r1) (NodeBT a2 l2 r2) = a1 == a2 && l1 == l2 && r1 == r2
    -- (==) _ _                                 = False
                                               
-- Tu ciąg dalszy...

a = NodeBT 2 EmptyBT EmptyBT
b = NodeBT 2 EmptyBT EmptyBT
c = NodeBT 5 EmptyBT EmptyBT
d = EmptyBT
e = EmptyBT
f = NodeBT 5 EmptyBT (NodeBT 2 EmptyBT EmptyBT)
g = NodeBT 5 EmptyBT (NodeBT 3 EmptyBT EmptyBT)