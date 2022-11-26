vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x,y,z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (i, c) = (c, i)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && y == z

heron :: (Double, Double, Double) -> Double
heron (a, b, c) = (sqrt((a+b+c)*(a+b-c)*(a-b+c)*(b+c-a)))/4

absInt :: Int -> Int
absInt a = if a >= 0 then a
            else -a

min2Int :: (Int, Int) -> Int
min2Int (a,b) = if a >= b then b
                else a

min3Int :: (Int, Int, Int) -> Int
min3Int (a,b,c) = if min2 <= c then min2 else c
                where min2 = if a <= b then a else b

min3Int_ :: (Int, Int, Int) -> Int
min3Int_ (a,b,c) = if min2 <= c then min2 else c
                where min2 = min2Int (a,b)

toUpper :: Char -> Char
toUpper a | ascii >= 97 && ascii <= 122 = toEnum (ascii - 32)::Char
          | otherwise = a
        where ascii = fromEnum a

toLower :: Char -> Char
toLower a | ascii >= 65 && ascii <= 90 = toEnum (ascii + 32)::Char
          | otherwise = a
        where ascii = fromEnum a

isDigit :: Char -> Bool
isDigit x | ascii >= 48 && ascii <= 57 = True
          | otherwise = False
        where ascii = fromEnum x

charToNum :: Char -> Int
charToNum x = fromEnum x

romanDigit :: Char -> String
romanDigit x
    | ascii == 49 = "I"
    | ascii == 50 = "II"
    | ascii == 51 = "III"
    | ascii == 52 = "IV"
    | ascii == 53 = "V"
    | ascii == 54 = "VI"
    | ascii == 55 = "VII"
    | ascii == 56 = "VIII"
    | ascii == 57 = "IV"
    | otherwise = "Error"
    where ascii = fromEnum x

sgn :: Int -> Int
sgn n | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

min3IntGuard :: (Int, Int, Int) -> Int
min3IntGuard (x,y,z) | x < y && x < z = x
                     | y < x && y < z = y
                     | z < x && z < y = z

or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' (x, y) = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (x,y) = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True) = False
nand' (x,y) = True

xor' :: (Bool, Bool) -> Bool
xor' (False, False) = False
xor' (True, True) = False
xor' (x,y) = True

isItTheAnswer :: String -> Bool
isItTheAnswer str = case str of
    "password" -> True
    _ -> False

not' :: Bool -> Bool
not' x = case x of
    True -> False
    False -> True


or'2 :: (Bool, Bool) -> Bool
or'2 (x, y) = case (x, y) of
    (False, False) -> False
    _ -> True

and'2 :: (Bool, Bool) -> Bool
and'2 (x, y) = case (x, y) of
    (True, True) -> True
    _ -> False

nand'2 :: (Bool, Bool) -> Bool
nand'2 (x, y) = case (x, y) of
    (True, True) -> False
    _ -> True
    
xor'2 :: (Bool, Bool) -> Bool
xor'2 (x, y) = case (x, y) of
    (True, True) -> False
    (False, False) -> False
    _ -> True

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x/len, y/len)
            where len = sqrt(x^2 + y^2)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = (x/len, y/len, z/len)
            where len = sqrt(x^2 + y^2 + z^2)

unitVec2Dlet :: (Double, Double) -> (Double, Double)
unitVec2Dlet (x, y) = let len = sqrt(x^2 + y^2) in (x/len, y/len)

unitVec3Dlet :: (Double, Double, Double) -> (Double, Double, Double) -- komentarz pierwszy
unitVec3Dlet (x, y, z) = let len = sqrt(x^2 + y^2 + z^2) in (x/len, y/len, z/len) {- komentarz wieloliniowy
bla bla
bla bla
-}



roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where { d = sqrt (b * b - 4 * a * c);
        e = 2 * a }

main :: IO()
main = do
    print(vec3DLen (1,1,1))
    print(swap (1, 'c'))
    print(threeEqual (1,1,2))
    print(heron (3,4,5))
    print(absInt (-5))
    print(min2Int (1,2))
    print(min3Int (1,2,-2))
    print(min3Int_ (1,2,-2))
    print(toUpper 'c')
    print(toLower 'B')
    print(isDigit '5')
    print(charToNum 'a')
    print(romanDigit '3')
    print(sgn (-6))
    print(min3IntGuard (1,2,3))
    print(or' (False, True))
    print(and' (False, True))
    print(nand' (False, True))
    print(xor' (False, True))

    print(or'2 (False, True))
    print(and'2 (False, True))
    print(nand'2 (False, True))
    print(xor'2 (False, True))
    print(unitVec2D (5,2))
    print(unitVec3D (5,2,1))
    
    print(unitVec2Dlet (5,2))
    print(unitVec3Dlet (5,2,1))
    print(roots (1,2,1))
