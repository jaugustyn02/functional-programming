{-# LANGUAGE DeriveFunctor #-}

doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

echo3' = do
    line1 <- getLine
    line2 <- getLine
    putStrLn (line1 ++ line2)


dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

twoQuestions :: IO ()
twoQuestions = putStr "What is your name? "
  >> getLine
  >>= \name -> putStr "How old are you? "
  >> getLine
  >>= \age -> print(name,age)


newtype Box a = MkBox a deriving (Show, Functor)

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)



data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
  fmap _ EmptyBT    = EmptyBT
  fmap f (NodeBT x btl btr) = NodeBT (f x) (fmap f btl) (fmap f btr)


newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
  fmap f (MyTriple (x, y, z)) = MyTriple (f x, f y, f z)

instance Applicative MyTriple where
  pure x = MyTriple (x, x, x)
  (MyTriple (f, i, j)) <*> w = fmap f w