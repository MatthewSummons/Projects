module Tutorial1 where

import Data.Char (ord)
import Prelude hiding (Maybe(..))



absolute :: Int -> Int
absolute x = if x < 0 then -x else x

-- | Q3. The tail of a list
--
-- Examples:
--
-- >>> tl [1,2,3]
-- [2,3]
--
-- >>> tl [1]
-- []
tl :: [a] -> [a]
tl [] = []
tl (x:xs) = xs


-- | Q4. Factorial function
--
-- Examples:
--
-- >>> factorial 3
-- 6
--
-- >>> factorial 0
-- 1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


-- | Q5. Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fibonacci 10
-- 55
--
-- >>> fibonacci 5
-- 5
fibonacci :: Int -> Int
fibonacci 0  = 0
fibonacci 1  = 1
fibonacci n  = fibonacci (n-1) + fibonacci (n-2)


-- -- | Q6.
-- -- >>> mapList absolute [4,-5,9,-7]
-- -- [4,5,9,7]
-- mapList :: (a -> b) -> [a] -> [b]
-- mapList f [] = []
-- mapList f (x:xs) = f x : mapList f xs



-- | Q7.
-- >>> ascii "abcds"
-- [97,98,99,100,115]
ascii :: [Char] -> [Int]
ascii = map ord



-- | Q8.
-- >>> filterList even [1,2,3,4,5]
-- [2,4]
filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (x:xs) = if f x
    then x : filterList f xs
    else filterList f xs



-- | Q9.
-- >>> zipList [1,2,3] ['a', 'b', 'c']
-- [(1,'a'),(2,'b'),(3,'c')]
zipList :: [a] -> [b] -> [(a,b)]
zipList (a:as) (b:bs) = (a, b) : zipList as bs
zipList [] bs = []
zipList as [] = []



-- | Q10.
-- >>> zipSum [1,2,3] [4,5,6]
-- [5,7,9]
zipSum :: [Int] -> [Int] -> [Int]
zipSum (a:as) (b:bs) = (a + b) : zipSum as bs
zipSum [] bs = []
zipSum as [] = []



data Maybe a = Nothing | Just a deriving Show

-- | Q11.
-- >>> safeHead []
-- Nothing
--
-- >>> safeHead [1,2,3]
-- Just 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x


-- | Q12.
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1,2]
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : catMaybes xs