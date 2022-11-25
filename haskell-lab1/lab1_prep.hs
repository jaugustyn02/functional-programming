prod :: Num a => [a] -> a -- prod [1, 2, 3] = 1 * 2* 3 = 6
prod [] = 1
prod (x:xs) = x * prod xs



check (x:xs:xd) = xd


length1 :: [a] -> Int -- length [1,1,1,1] = 4
length1 = loop 0
    where loop acc [] = acc
          loop acc (_:xs) = loop (acc + 1) xs