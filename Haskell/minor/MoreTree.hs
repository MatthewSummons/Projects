module MoreTree where

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Act of Left, Root &  Right node with the base case for the leaf
foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree f z Leaf = z
foldTree f z (Node l a r) = f (foldTree f z l) a (foldTree f z r)

treeSize :: Tree a -> Int
treeSize = foldTree (\l _ r -> l + 1 + r) 0

-- Tree is complete if *every* node's subtrees are of equal size
isComplete :: Tree a -> Bool
isComplete Leaf = True
isComplete (Node l _ r) = (treeSize l == treeSize r) && isComplete l && isComplete r

-- Post Order :: Left -> Right -> Root
foldTreePos :: (a -> b -> b) -> b -> Tree a -> b
foldTreePos f z Leaf = z
foldTreePos f z (Node l a r) = f a (foldTreePos f (foldTreePos f z l) r)

-- Pre Order :: Root -> Left -> Right
foldTreePre :: (a -> b -> b) -> b -> Tree a -> b
foldTreePre f z Leaf = z
foldTreePre f z (Node l a r) = foldTreePre f (foldTreePre f (f a z) l) r

-- In Order :: Left -> Root -> Right
foldTreeIn :: (a -> b -> b) -> b -> Tree a -> b
foldTreeIn f z Leaf = z
foldTreeIn f z (Node l a r) = foldTreeIn f (f a (foldTreeIn f z l)) r


t0 = Leaf

t1 = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

t2 = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 4 Leaf) 3 Leaf)

t3 =
  Node
    ( Node
        ( Node
            ( Node
                Leaf
                5
                Leaf
            )
            4
            Leaf
        )
        2
        Leaf
    )
    1
    ( Node
        ( Node
            Leaf
            6
            Leaf
        )
        3
        ( Node
            Leaf
            7
            Leaf
        )
    )
