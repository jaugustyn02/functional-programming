import Data.List

-- 5) Operator złożenia funkcji (.) (i notacja point-free)

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc' :: Ord a => [a] -> [a]
sortDesc' xs = reverse (sort xs)

