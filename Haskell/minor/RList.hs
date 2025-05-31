module RList where

-- Random Access Lists implementation with O(log i) indexing time
-- See reference A2 for details

import Data.List
import Data.Maybe

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)

newtype RList a = RList [Tree a] deriving (Show, Eq)

nil :: RList a
nil = RList []

-- [RList.Size - O(1)]
size :: Tree a -> Int
size (Leaf _) = 1
size (Node len _ _) = len

-- [RList.Len - O(n)]
len :: RList a -> Int
len (RList xs) = sum (map size xs)

-- [RList.InsertTree]
insertTree :: Tree a -> [Tree a] -> [Tree a]
insertTree t [] = [t]
insertTree t (t' : ts') =
  if size t > size t'
    then error "Placing a larger tree before a smaller tree!"
    else fix (t : t' : ts')

{-  Check the invariance of the RList, namely ensuring that size of adjacent elements
    are in a strictly increasing order. If two adjacent elements are of the same size,
    merge them together and re-verify the invariance. If the invariance is not violatded
    return the original list. Note that the invariance can only be violated if there are
    are at least two elements in the RList      -}
fix :: [Tree a] -> [Tree a]
fix rlist@(t : t' : ts') =
  if invarianceViolated
    then case splitAt (fromJust index) rlist of
      (x, y : z : zs) -> fix (x ++ merge y z : zs)
    else rlist
  where
    elemPairs = [size x >= size y | (x, y) <- zip rlist (tail rlist)]
    invarianceViolated = or elemPairs
    index = findIndex id elemPairs -- At which the invariance is violated
fix ts = ts

-- Take a two complete binary trees of equal size and merge them into a single complete binary tree
merge :: Tree a -> Tree a -> Tree a
merge t@(Leaf a) t'@(Leaf b) = Node 2 t t'
merge (Node n s t) (Node n' s' t') =
  if n == n'
    then Node (2 * n) (merge s t) (merge s' t')
    else error "Cannot merge trees of different sizes!"

cons :: a -> RList a -> RList a
cons x (RList xs) = RList $ insertTree (Leaf x) xs

fromList :: [a] -> RList a
fromList = foldr cons nil

-- [RList.SplitTree]
splitTree :: Tree a -> (a, [Tree a])
splitTree (Leaf a) = (a, [])
splitTree (Node n l r) =
  if size l == size r
    then split l [r]
    else error "Detected ill-formed (non-complete) tree!"

split :: Tree a -> [Tree a] -> (a, [Tree a])
split (Leaf a) ts = (a, fix ts)
split (Node 2 (Leaf a) r') ts = (a, r' : ts)
split (Node _ (Node _ l r) r') ts = case split l [r] of
  (a, ts) -> (a, ts ++ [r'])

-- [RList.Uncons]
uncons :: RList a -> (a, RList a)
uncons (RList []) = error "Empty RList!"
uncons (RList (t : ts)) = case splitTree t of
  (a, ts') -> (a, RList (ts' ++ ts))

-- [RList.ixTree]
ixTree :: Tree a -> Int -> a
ixTree (Leaf a) i =
  if abs i > 0
    then error ("Index " ++ show i ++ " out of range!")
    else a
ixTree (Node n l r) i =
  if i < n
    then
      if i < n `div` 2
        then ixTree l i
        else ixTree r (i - n `div` 2)
    else error ("Index " ++ show i ++ " out of range!")

-- [RList.ix]
ix :: RList a -> Int -> a
ix (RList []) i = error "Index out of range!"
ix (RList (t : ts)) i =
  if i < size t
    then ixTree t i
    else ix (RList ts) (i - size t)