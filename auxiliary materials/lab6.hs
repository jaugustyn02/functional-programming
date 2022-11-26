import Control.Monad

(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x

infixl 0 >$>


(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>


safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

-- nie da się złożyć bo zwracają różne typy

extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just x) = x


insertMaybe :: a -> Maybe a
insertMaybe = Just

-- (>^$>) = extract (^) and apply ($)
-- (>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- ma >^$> f = (extractMaybe ma) >$> f
-- infixl 1 >^$>
-- nie przewiduje dostania argumentu Nothing


(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >^$> _ = Nothing
(Just x) >^$> f = f x
infixl 1 >^$>
-- jest opcja otrzymania Nothing na wejściu


f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

-- Kleisli composition
(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> g (extractMaybe (f x))
f >.>> g = \x -> (f x)  >^$> g



doSafeTail3x :: [a] -> Maybe [a]
doSafeTail3x xs = do
  t1 <- safeTail xs
  t2 <- safeTail t1
  t3 <- safeTail t2
  return t3

safeTail3x :: [a] -> Maybe [a]
safeTail3x xs =
  safeTail xs >>= \t1 ->
    safeTail t1 >>= \t2 ->
      safeTail t2 >>= \t3 ->
        return t3

safeTail3x' :: [a] -> Maybe [a]
safeTail3x' xs = return xs >>= safeTail >>= safeTail >>= safeTail
-- najczytelniesza ostatnia


f5 :: Int -> Int -> Int -> Int
f5 x y z = 1000 `div` x + 100 `div` y + 10 `div` z


safeDiv :: Int -> Int -> Maybe Int
safeDiv x y | y /= 0    = Just $ x `div` y
            | otherwise = Nothing

safeF5 :: Int -> Int -> Int -> Maybe Int
safeF5 x y z =
  case (safeDiv 1000 x) of
    Nothing -> Nothing
    Just (iOverX) ->
      case (safeDiv 100 y) of
        Nothing -> Nothing
        Just (iOverY) ->
          case (safeDiv 10 z) of
            Nothing -> Nothing
            Just (iOverZ) -> Just $ iOverX + iOverY + iOverZ


safeF5' :: Int -> Int -> Int -> Maybe Int
safeF5' x y z = do
  iOverX <- safeDiv 1000 x
  iOverY <- safeDiv 100 y
  iOverZ <- safeDiv 10 z
  return $ iOverX + iOverY + iOverZ	

(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >=> g = \x -> f x >>= g

join :: Maybe (Maybe a) -> Maybe a
join (Just (Just a)) = Just a
join (Just Nothing)  = Nothing
join Nothing = Nothing

xs1 :: [(Int,Int,Int)]
xs1 = [ (x,y,z) | let xs = [1,2], x <- xs, y <- xs, z <- xs ]

doXs1 :: [(Int,Int,Int)]
doXs1 = do
  let xs = [1,2]
  x <- xs
  y <- xs
  z <- xs
  return (x,y,z)

xs2 :: [(Int,Int,Int)]
xs2 = [ (x,y,z) | let xs = [1..3], x <- xs, y <- xs, z <- xs, x > y && y > z ]

doXs2 :: [(Int,Int,Int)]
doXs2 = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs
  guard $ x > y && y > z
  return (x,y,z)

doXs2' :: [(Int,Int,Int)]
doXs2' = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs
  if x > y && y > z
    then return (x,y,z)
    else []

join' :: [[a]] -> [a]
join' (x:xs) = x ++ join' xs
join' [] = []