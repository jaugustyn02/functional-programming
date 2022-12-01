-- 1.png
-- data T1 a b c = C a (b,c) | D (a,b)

-- instance (Eq a, Eq b, Eq c) => Eq(T1 a b c) where
--     (==) (C x1 (y1,z1)) (C x2 (y2,z2)) = x1 == x2 && y1 == y2 && z1 == z2
--     (==) (D (x1,y1)) (D (x2,y2)) = x1 == x2 && y1 == y2
--     (==) _ _ = False

-- 2.png
-- data T1 = T1_C1 (Bool, Double) | T1_C2

-- instance Eq T1 where
--     (==) (T1_C1 (b1, d1)) (T1_C1 (b2,d2)) = b1 == b2 && d1 == d2
--     (==) (T1_C2) (T1_C2)                  = True
--     (==) _ _                              = False

-- 3.png Abstrakcyjny typ danych (ADT)
-- b) którego specyfikacja jest ograniczona do związanych z nim operacji z całkowitym pominięciem szczegółów reprezentacji/implementacji

-- 4.png
-- data BST a = EmptyBST | NodeBST (BST a) a (BST a)

-- insert2BST :: Ord a => a -> BST a -> BST a
-- insert2BST x EmptyBST = NodeBST EmptyBST x EmptyBST
-- insert2BST x (NodeBST lt e rt)
--     | x == e = NodeBST lt e rt
--     | x < e = NodeBST (insert2BST x lt) e rt
--     | otherwise = NodeBST lt e (insert2BST x rt)

-- 5.png
-- data BinTree a = EmptyBT | NodeBT (BinTree a) a (BinTree a)

-- mapBT :: (a -> b) -> BinTree a -> BinTree b
-- mapBT f EmptyBT = EmptyBT
-- mapBT f (NodeBT lt x rt) = NodeBT (mapBT f lt) (f x) (mapBT f rt)

-- 6.png
-- data BinTree a = EmptyBT | NodeBT (BinTree a) a (BinTree a)
-- t1 = NodeBT (NodeBT EmptyBT 1 EmptyBT) 1 (NodeBT EmptyBT 1 EmptyBT)

-- 7.png
-- data BinTree a = Leaf a | NodeBT (BinTree a) (BinTree a)

-- flattenBT :: BinTree a -> [a]
-- flattenBT (Leaf x) = [x]
-- flattenBT (NodeBT lt rt) = (flattenBT lt) ++ (flattenBT rt)

-- 8.png
-- data T1 a b = B (a,b) | A

-- instance (Eq a, Eq b) => Eq (T1 a b) where
--     (==) (B (a1,b1)) (B (a2,b2)) = a1 == a2 && b1 == b2
--     (==) A A = True
--     (==) _ _ = False

-- 9.png
-- data BinTree a = Leaf a | NodeBT (BinTree a) (BinTree a)

-- mapBT :: (a -> b) -> BinTree a -> BinTree b
-- mapBT f (Leaf x) = Leaf (f x)
-- mapBT f (NodeBT lt rt) = NodeBT (mapBT f lt) (mapBT f rt)

-- 10.png
-- data BinTree a = NodeBT (BinTree a) (BinTree a) | Leaf a
-- t1 = NodeBT (Leaf 1) (Leaf 1)

-- 11.png
-- data BST a = EmptyBST | NodeBST a (BST a) (BST a)

-- isElemOfBST :: Ord a => a -> BST a -> Bool
-- isElemOfBST _ EmptyBST = False
-- isElemOfBST x (NodeBST e lt rt)
--     | x == e = True
--     | x < e = isElemOfBST x rt
--     | otherwise = isElemOfBST x lt

-- 12.png - Algebraiczny typ danych:
-- b) który jest zbudowany z innych typów przez użycie dwóch operacji określonych na poziomie typu - dodwania i mnożenia

-- 13.png
-- data T1 = T1_C1 Int | T1_C2 Double Bool

-- instance Eq T1 where
--     (==) (T1_C1 i1) (T1_C1 i2) = i1 == i2
--     (==) (T1_C2 d1 b1) (T1_C2 d2 b2) = d1 == d2 && b1 == b2
--     (==) _ _ = False