-- 1.
data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

-- 2.
data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

-- 3.
-- data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
--               Add (Expr a) (Expr a)

-- eval :: Num a => Expr a -> a
-- eval (Lit n) = n
-- eval (Add e1 e2) = eval e1 + eval e2

-- show' :: Show a => Expr a -> String
-- show' (Lit n) = show n
-- show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

-- 4.
bt = NodeBT 3 (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 5 EmptyBT EmptyBT)) (NodeBT 4 EmptyBT EmptyBT)

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

preflattenBT :: BinTree a -> [a] -- preorder
preflattenBT EmptyBT = []
preflattenBT (NodeBT n lt rt) = n : (preflattenBT lt) ++ (preflattenBT rt)

inflattenBT :: BinTree a -> [a] -- inorder
inflattenBT EmptyBT = []
inflattenBT (NodeBT n lt rt) = (inflattenBT lt) ++ n : (inflattenBT rt)

postflattenBT :: BinTree a -> [a] -- postorder
postflattenBT EmptyBT = []
postflattenBT (NodeBT n lt rt) = (postflattenBT lt) ++ (postflattenBT rt) ++ [n]

-- preflattenBT bt -> [3,1,2,5,4]
-- inflattenBT bt -> [2,1,5,3,4]
-- postflattenBT bt -> [2,5,1,4,3]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert x EmptyBT = (NodeBT x EmptyBT EmptyBT)
insert x (NodeBT n lt rt) = (if x < n then NodeBT n (insert x lt) rt
                             else NodeBT n lt (insert x rt))

list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

-- 6.
data Expr a = Lit a |
              Add (Expr a) (Expr a) |
              Multiply (Expr a) (Expr a) |
              Subtract (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Multiply e1 e2) = eval e1 * eval e2
eval (Subtract e1 e2) = eval e1 - eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Add e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"
show' (Add e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
