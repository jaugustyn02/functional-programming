-- {-# LANGUAGE DeriveFunctor #-}
-- 1.
-- (filter odd) <$> Left [1..3]
-- Left [1,2,3]

-- 2.
-- (\x y -> max <$> x <*> y) <$> [Just 1, Just 2] <*> [Just 3, Nothing]
-- [Just 3,Nothing,Just 3,Nothing]

-- 3.
-- (\x y z -> (x == y, z == y)) <$> Right 1 <*> Right 2 <*> Left 2
-- Left 2

-- 4.
-- pure (<) <*> Right 1 <*> pure 3
-- Right True

-- 5. nie dziala
-- data Either' e a = Left' e | Right' a

-- instance Functor (Functor e, Functor a) => (Either' e a)  where
--     fmap g (Left' e)    = Left' (g e)
--     fmap g (Right' x)   = Right' (g x)


-- 6. nie dziala
-- data App a = [] | a:[a]

-- instance Applicative (Applicative a => [a]) where
--     pure x = [x]
--     fs <*> xs = 
--         [f x | f <- fs, x <- xs]
-- 7.
-- odd <$> [1..4]
-- [True,False,True,False]
-- 8.
g1 = getLine >>= \l1 -> return (l1 ++ l1) >>= \l2 -> print[l1,l2]


g2 = do 
    l1 <- getLine
    l2 <- return (l1 ++ l1)
    print [l1,l2]
