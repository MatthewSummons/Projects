module InfStream where

-- | Factorials
--
-- >>> take 10 facts
-- [1,2,6,24,120,720,5040,40320,362880,3628800]

facts :: [Integer]
facts = 1 : zipWith (*) facts [2..]


-- | Merge
--
-- >>> merge [1,3,6,34,234] [2,3,5,12,21,23,34,565,3442]
-- [1,2,3,5,6,12,21,23,34,234,565,3442]

merge :: [Integer] -> [Integer] -> [Integer]
merge []     bs     = bs
merge as     []     = as
merge (a:as) (b:bs)
    | a < b     = (a:merge as (b:bs))
    | a > b     = (b:merge (a:as) bs)
    | otherwise = (a:merge as bs)

-- | Prime factors 2, 3, 5
--
-- >>> take 10 s
-- [1,2,3,4,5,6,8,9,10,12]

s :: [Integer]
s = 1 : merge (merge (map (*2) s) (map (*3) s)) (map (*5) s)
