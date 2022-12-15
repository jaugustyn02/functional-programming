{-# LANGUAGE DeriveFunctor #-}
-- 1. 2.
newtype Box a = MkBox a deriving (Show, Functor)
-- newtype Box a = MkBox a deriving Show

-- instance Functor Box where
--   fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)

-- instance Functor MyList where
--   fmap _ EmptyList    = EmptyList
--   fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)


-- data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

-- instance Functor BinTree where
--     fmap _ EmptyBT = EmptyBT
--     fmap f (NodeBT a btl btr) = NodeBT (f a) (fmap f btl) (fmap f btr)


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show,Functor)