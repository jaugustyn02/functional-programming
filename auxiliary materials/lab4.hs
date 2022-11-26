type T1 a = Int
f1 :: T1 x -> T1 x
f1 x = x + 2
newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

a = MkPolarCoord'' (1,1)
a1 = polarToCartesian'' a

b = MkPolarCoord'' (-1,-1)
b1 = polarToCartesian'' b   -- wszystko działa nawet dla niepoprawnych danych w dobrym typie, nie działa dla nieprawidłowych typów

data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

data Cart3DVec a = Cart3DVec a a a

xCoord3 :: Cart3DVec a -> a
xCoord3 (Cart3DVec x _ _) = x

yCoord3 :: Cart3DVec a -> a
yCoord3 (Cart3DVec _ y _) = y

zCoord3 :: Cart3DVec a -> a
zCoord3 (Cart3DVec _ _ z) = z

-- *Main> c = Cart3DVec 1 2 3
-- *Main> xCoord3 c
-- *Main> yCoord3 c
-- *Main> zCoord3 c

data Cart3DVecRS a = Cart3DVecRS {x::a, y::a, z::a}

-- *Main> x a
-- 1
-- *Main> y a
-- 2
-- *Main> z a
-- 3

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle x y) = x*y

-- *Main> circle = Circle 1
-- *Main> area circle
-- 3.1415927
-- *Main> rec = Rectangle 2 4
-- *Main> area rec
-- 8.0

data TrafficLights = Red | Green | Yellow

actionFor :: TrafficLights -> String
actionFor Red = "Zatrzymaj się"
actionFor Yellow = "Dodaj gazu, zeby zdazyc ;)"
actionFor Green = "Jedź śmiało"
-- *Main> actionFor Yellow

data BinTree a = EmptyBT |
                  NodeBT a (BinTree a) (BinTree a)


depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = max (1 + depthOfBT lt) (1 + depthOfBT rt)


-- *Main> depthOfBT $ NodeBT 1 EmptyBT EmptyBT
-- 1
--depthOfBT $ NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT)

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = [n] ++ flattenBT lt ++ flattenBT rt    --rosnąco

flattenBT' :: BinTree a -> [a]
flattenBT' EmptyBT = []
flattenBT' (NodeBT n lt rt) = flattenBT' rt ++ flattenBT' lt ++ [n]     --malejąco

-- flattenBT $ NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT)

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT func (NodeBT n lt rt) = (NodeBT (func n) (mapBT func lt) (mapBT func rt))


insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert x EmptyBT = (NodeBT x EmptyBT EmptyBT)
insert x (NodeBT n lt rt) = (if x < n then insert x lt else insert x rt)

list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

instance Eq a => Eq (BinTree a) where
  (==) (EmptyBT) (EmptyBT) = True
  (==) (NodeBT n1 lt1 rt1) (NodeBT n2 lt2 rt2) = n1 == n2 && lt1 == lt2 && rt1 == rt2


data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Multiply (Expr a) (Expr a) |
              Subtract (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Multiply e1 e2) = eval e1 + eval e2
eval (Subtract e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Multiply e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"
show' (Subtract e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"

newtype MyInt = MkMyInt Int

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
--działa