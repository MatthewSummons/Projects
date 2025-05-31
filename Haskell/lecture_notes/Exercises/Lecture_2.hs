module Lecture_2 where

import Prelude hiding (last, init)
-- 2. Inconsistent indent
-- 3. Last
last :: [a] -> a
last xs = head (reverse xs)
-- 4. Last (Alternate)
last' :: [a] -> a
last' [] = error "Cannot take the last element of an empty list"
last' [x] = x
last' (x:xs) = last xs

-- 5. Init
init :: [a] -> [a]
init xs = tail (reverse xs)

init' :: [a] -> [a]
init' [] = error "Cannot remove the last element from an empty list"
init' [x] = []
init' (x:xs) = x : init' xs

