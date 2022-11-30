-- module Stack
--   ( Stack
--   , empty   -- :: Stack a
--   , isEmpty -- :: Stack a -> Bool
--   , push    -- :: a -> Stack a -> Stack a
--   , top     -- :: Stack a -> a
--   , pop     -- :: Stack a -> (a,Stack a)
--   ) where
--         -- interface (signature, contract)
--         empty :: Stack a
--         isEmpty :: Stack a -> Bool
--         push :: a -> Stack a -> Stack a
--         top :: Stack a -> a
--         pop :: Stack a -> (a,Stack a)
--         -- implementation
--         newtype Stack a = MkStack [a] deriving Show -- hidden constructor (see the module export list)
--         empty = MkStack []
--         isEmpty (MkStack s) = null s
--         push x (MkStack s) = MkStack (x:s)
--         top (MkStack s) = head s
--         pop (MkStack (s:ss)) = (s,MkStack ss)

module Queue
  ( Queue
  , emptyQ
  , isEmptyQ
  , addQ
  , remQ
  ) where

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a, Queue a)

newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue []
isEmptyQ (MkQueue q) = null q
addQ x (MkQueue q) = MkQueue (q++[x])
remQ (MkQueue (x:q)) = (x, MkQueue q)

