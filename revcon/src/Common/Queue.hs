module Common.Queue where

import Data.List (intercalate)

-- Data structure
data Queue a = Queue [a] [a]

-- Instances
instance Show a => Show (Queue a) where
  show q = "[>" ++ intercalate " > " (map show $ toList q) ++ ">]"

instance Eq a => Eq (Queue a) where
  x1 == x2 = toList x1 == toList x2

instance Functor Queue where
  fmap f (Queue e d) = Queue (fmap f e) (fmap f d)

instance Foldable Queue where
  foldr f x (Queue e d) = foldr f x (d ++ reverse e)

-- Functions
empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _             = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue e d) = Queue (x:e) d

unenqueue :: Queue a -> (Maybe a, Queue a)
unenqueue (Queue [] []) = (Nothing, Queue [] [])
unenqueue (Queue e d)
  | length e < length d = unenqueue (Queue (e ++ reverse d) [])
  | otherwise           = (Just (head e), Queue (tail e) d)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue [] [])  = (Nothing, Queue [] [])
dequeue (Queue e d)
 | length e > length d = dequeue (Queue [] (d ++ reverse e))
 |  otherwise          = (Just (head d),Queue e (tail d))

undequeue :: a -> Queue a -> Queue a
undequeue x (Queue e d) = Queue e (x:d)

fromList :: [a] -> Queue a
fromList e = Queue e []

toList :: Queue a -> [a]
toList (Queue e d) = e ++ reverse d
