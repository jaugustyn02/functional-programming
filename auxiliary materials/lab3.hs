import Data.List

f1 = \x -> x-2
f2 = \x y -> sqrt (x^2 + y^2)
f3 = \x y z -> sqrt (x^2 + y^2 + z^2)
f4 = \x -> 2*x
f5 = \x -> x*2
f6 = \x -> 2^x
f7 = \x -> x^2
f8 = \x -> 2/x
f9 = \x -> x/3
f10 = \x -> 4-x

f11 = \x -> case x `mod` 2 of
            0 -> True
            _ -> False

f12 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f13 = \x -> case x of
            1 -> 3
            _ -> 0

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

cube = \x -> x^3
abs2 = \x -> case x >= 0 of
                True -> x
                False -> -x

sum2 x = sumWith (\a -> a) x
sumSqr x = sumWith f7 x
sumCube x = sumWith cube x
sumAbs x = sumWith abs2 x
sumWidth x = sumWith (\x -> 1) x

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of
    0 -> \x -> 1 + x
    1 -> \x -> 1 + x + (x^2)/2
    2 -> \x -> 1 + x + (x^2)/2 + (x^3)/6
    3 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24
    4 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24 + (x^5)/120
    5 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24 + (x^5)/120 + (x^6)/720

silnia :: Double -> Double
silnia 0 = 1
silnia n = n * silnia (n - 1)

expApproxUpTo2 :: Int -> Double -> Double
expApproxUpTo2 0 = \x -> 1 + x
expApproxUpTo2 n = \x -> (x^(n+1))/(silnia (nDouble + 1)) + (expApproxUpTo2 (n-1)) x where nDouble = fromIntegral n

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc2 :: Ord a => [a] -> [a]
sortDesc2 xs = reverse (sort xs)

f = (+1)
g = (*2)
h = (^3)
w3 = \x y z -> sqrt (x^2 + y^2 + z^2)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs


onlyEven = filter' (\x -> x `mod` 2 == 0)
onlyOdd = filter' (\x -> x `mod` 2 == 1)

onlyUpper :: Integral a => [a] -> [a]
onlyUpper = filter' (\x -> let ascii = fromEnum x in ascii >= 65 && ascii <= 90)


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

doubleElems = map' (\x -> x*2)
sqrElems    = map' (\x -> x^2)

lowerCase :: [Char] -> [Char]
lowerCase   = map' (\x -> let ascii = fromEnum x in 
    case ascii >= 65 && ascii <= 90 of
        True -> toEnum (ascii + 32)
        False -> x
    )

doubleElems' xs = [i*2 | i <- xs]
sqrElems' xs = [i^2 | i <- xs]
lowerCase' xs = [let ascii = fromEnum a in if ascii >= 65 && ascii <= 90 then toEnum (ascii + 32) else a | a <- xs]


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr f z xs)

sumWith'' g = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f z []     = z
foldl'' f z (x:xs) = foldl f (f z x) xs

sumWith''' g  = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 1

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldl1 (&&) (zipWith (<=) xs (tail xs))

everySecond :: [t] -> [t]
everySecond [] = []
everySecond (x1:x2:xs) = x1 : everySecond xs


-- main :: IO()
-- main = do
    -- print(f1 5)
    -- print(f2 3 4)
    -- print(f3 3 4 5)
    -- print(f4 9)
    -- print(f5 9)
    -- print(f6 9)
    -- print(f7 9)
    -- print(f8 9)
    -- print(f9 9)
    -- print(f10 9)
    -- print(f11 4)
    -- print(f12 4)
    -- print(f13 1)
    -- print(sumSqr' [1,2,3,4,5])
    -- print(sumWith f7 [1,2,3,4,5])
    -- print(sum2 [1,2,3,4,5])
    -- print(sumSqr [1,2,3,4,5])
    -- print(sumCube [1,2,3,4,5])
    -- print(sumAbs [1,-2,3,4,-5])
    -- print(sumWith (\x -> x^5) [1..15])
    -- print(sumWidth [1..9])
    -- print(expApproxUpTo 5 1)
    -- print(expApproxUpTo2 5 1)
    -- print(sortDesc [2,5,3,7,8,1])
    -- print(sortDesc2 [2,5,3,7,8,1])
    -- print((f . w3 4 5 . h) 3)
    --((,) $ 1) $ 2
    --najbardziej czytelne ---> (f . g . h) 3
    --szybsze z filter z biblioteki standardowej
    --length $ onlyEven [1..10^6]
    --length $ filter even [1..10^6]
    --length [i | i <- [1..10^6], i `mod` 2 == 0]
    --szybsze ----> length . filter even . map (*2) $ [1..10^7] -- map z biblioteki standardowej


    