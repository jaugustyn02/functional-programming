min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x < y
        then x
        else y


min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = min2Int(x, min2Int(y, z))
