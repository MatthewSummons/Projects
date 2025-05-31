module Tree where

-- Dataypes [P3]

data Tree = Empty | Node Int Tree Tree deriving (Show)

isBalanced :: Tree -> Bool
isBalanced Empty = True
isBalanced (Node _ l r) =
  abs (depth l - depth r) <= 1 && isBalanced l && isBalanced r
  where
    depth :: Tree -> Int
    depth Empty = 0
    depth (Node _ l r) = 1 + max (depth l) (depth r)

fromList :: [Int] -> Tree
fromList = foldr insert Empty

insert :: Int -> Tree -> Tree
insert x Empty = Node x Empty Empty
insert x (Node c l r)
  | x < c = Node c (insert x l) r
  | otherwise = Node c l (insert x r)

isBST :: Tree -> Bool
isBST Empty = True
isBST (Node c l r) = isBST l && isBST r && c > maxT l && c <= minT r

maxT :: Tree -> Int
maxT Empty = minBound
maxT (Node c l r) = maximum [c, maxT l, maxT r]

minT :: Tree -> Int
minT Empty = maxBound
minT (Node c l r) = minimum [c, minT l, minT r]