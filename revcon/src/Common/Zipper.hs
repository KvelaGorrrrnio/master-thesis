module Common.Zipper where

import Data.List (intercalate)

-- Data structure
data Zipper a = Zipper ![a] ![a] deriving (Eq)

-- Instances
instance Show a => Show (Zipper a) where
  show (Zipper l r) = "\n[" ++ intercalate " | " (map show (reverse l)) ++ "] <|> [" ++ intercalate " | " (map show r) ++ "]\n"

instance Functor Zipper where
  fmap f (Zipper l r) = Zipper (fmap f l) (fmap f r)

instance Foldable Zipper where
  foldr f x (Zipper l r) = foldr f x (l ++ r)

-- Function declarations
empty :: Zipper a
empty = Zipper [] []

emptyz :: Zipper a -> Bool
emptyz (Zipper [] []) = True
emptyz _              = False

fromList :: [a] -> Zipper a
fromList = Zipper []

toList :: Zipper a -> [a]
toList (Zipper l r) = reverse l ++ r

left :: Zipper a -> Zipper a
left (Zipper [] _)    = error "Zipper.left: No more elements"
left (Zipper (x:l) r) = Zipper l (x:r)

leftN :: Int -> Zipper a -> Zipper a
leftN 0 z = z
leftN n z
  | beginz z  = z
  | otherwise = leftN (n-1) (left z)

rightN :: Int -> Zipper a -> Zipper a
rightN 0 z = z
rightN n z
  | endz z  = z
  | otherwise = rightN (n-1) (right z)

right :: Zipper a -> Zipper a
right (Zipper _ []) = error "Zipper.right: No more elements"
right (Zipper l (x:r)) = Zipper (x:l) r

cursor :: Zipper a -> Maybe a
cursor (Zipper _ []) = Nothing
cursor (Zipper _ (x:_)) = Just x

precursor :: Zipper a -> Maybe a
precursor (Zipper [] _) = Nothing
precursor (Zipper (x:_) _) = Just x

insert :: a -> Zipper a -> Zipper a
insert x (Zipper l r) = Zipper l (x:r)

delete :: Zipper a -> Zipper a
delete (Zipper _ []) = error "Zipper.delete: No element under cursor"
delete (Zipper l (_:r)) = Zipper l r

replace :: a -> Zipper a -> Zipper a
replace x (Zipper l (_:r)) = Zipper l (x:r)

end :: Zipper a -> Zipper a
end (Zipper l r) = Zipper (reverse r ++ l) []

begin :: Zipper a -> Zipper a
begin (Zipper l r) = Zipper [] (reverse l ++ r)

endz :: Zipper a -> Bool
endz (Zipper _ []) = True
endz _             = False

beginz :: Zipper a -> Bool
beginz (Zipper [] _) = True
beginz _             = False

takez :: Int -> Zipper a -> (Int, Zipper a)
takez n (Zipper _ r) = let r' = take n r in (length r', Zipper [] r')

reversez :: Zipper a -> Zipper a
reversez (Zipper l r) = Zipper r l

mapz :: (a -> b) -> Zipper a -> Zipper b
mapz f (Zipper l r) = Zipper (map f l) (map f r)
