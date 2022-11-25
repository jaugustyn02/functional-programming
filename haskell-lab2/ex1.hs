myFun x = 2 * x


add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y

add2C :: Num a => a -> (a -> a) -- lewostronnie łączny
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x, y, z) = x + y + z

add3C :: Num a => a -> (a -> (a -> a))
add3C x y z = x + y + z

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = flip (-) 5
