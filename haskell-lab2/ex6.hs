fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)



sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs


-- 7. zad

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' []        = 1
prod' (x:xs)    = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' (x:xs) = if x then True else or' xs

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = True
and' (x:xs) = if not x then False else and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' e [] = False
elem' e (x:xs) = if x == e then True else elem' e xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = x*2 : doubleAll xs

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = x^2 : squareAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs) = if (x `mod` 2 == 1) then selectEven xs else x : selectEven xs


-- 7.2
sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs


sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs


prod'2 :: Num a => [a] -> a
prod'2 = loop 1
 where loop acc []      = acc
       loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 = loop 0
 where loop acc [] = acc
       loop acc (x:xs) = loop (acc + 1) xs