module Lecture_3 where

import Prelude hiding ((||), (&&))

-- 1. SafeTail
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
    | null xs   = []
    | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

-- 2. Logical OR [2 Versions already enough]

(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _ _ = False

(|||) :: Bool -> Bool -> Bool
(|||) True _ = True
(|||) _ True = True
(|||) _ _    = False

-- 3. Redefine (&&)
(&&) :: Bool -> Bool -> Bool
(&&) x y = if x == True 
    then (if y == True then True else False)
    else False

-- 4. Redefine (&&) again
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x == True then y else False
