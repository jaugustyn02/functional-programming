fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

div12 :: Integral a => [a] -> Bool
div12 (x : y : _) | (y `mod` x) == 0 = True
div12 _                              = False