-- 1.
-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

-- 2.
data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

-- 3,4.
-- data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y

-- 5.
-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- 6.
-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

-- 7.
{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a

xCoord3 :: Cart3DVec a -> a
xCoord3 (Cart3DVec x _ _) = x

yCoord3 :: Cart3DVec a -> a
yCoord3 (Cart3DVec _ y _) = y

zCoord3 :: Cart3DVec a -> a
zCoord3 (Cart3DVec _ _ z) = z

-- *Main> c = Cart3DVec 1 2 3
-- *Main> xCoord3 c
-- *Main> yCoord3 c
-- *Main> zCoord3 c

data Cart3DVecRS a = Cart3DVecRS {x::a, y::a, z::a}

-- let p123 = Cart3DVecRS 1 2 3
-- let p123 = Cart3DVecRS {x=1,y=2,z=3}

-- x p123
-- y p123
-- z p123

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b



data TrafficLights = Green | Yellow | Redd

actionFor :: TrafficLights -> String
actionFor Green = "Jedź"
actionFor Yellow = "Jedź szybciej"
actionFor Redd = "Zapierdalaj"
