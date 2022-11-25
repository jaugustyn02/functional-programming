-- 2) Funkcje wyższego rzędu: funkcje jako parametry/argumenty
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f []     = 0
sumWith f (x:xs) = f(x) + sumWith f xs

sum = sumWith (\x -> x)
sumSqr  = sumWith (^2)
sumCube = sumWith (^3)
sumAbs  = sumWith (\x -> case x >= 0 of
                    True -> x
                    False -> -x)

ta_suma = sumWith (^5) [1..15]

listLength = sumWith (^0)
listLength' = sumWith (\x -> 1)