module Interleave where

{-
    InterleaveSum [P2]

    Write a function interleaveSum :: [Int] -> [Int] -> [Int] that takes two lists of
    integers and interleaves the sum of the elements at the same position in both lists.
    If one list is longer than the other, the remaining elements should be
    included in the output as it is.
-}

interleaveSum :: (Num a) => [a] -> [a] -> [a]
interleaveSum xs [] = xs
interleaveSum [] ys = ys
interleaveSum (x : xs) (y : ys) = (x + y) : interleaveSum xs ys

interleaveSum' :: (Num a) => [a] -> [a] -> [a]
interleaveSum' as bs = [a + b | (a, b) <- take m (zip (as ++ repeat 0) (bs ++ repeat 0))]
  where
    m = max (length as) (length bs)

interleaveSum'' :: Num a => [a] -> [a] -> [a]
interleaveSum'' as bs =
  foldr (\(x, y) ls -> (x + y) : ls) [] (take m (zip (as ++ repeat 0) (bs ++ repeat 0)))
  where
    m = max (length as) (length bs)