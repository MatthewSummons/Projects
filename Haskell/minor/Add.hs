module Add where

-- Add arbitrarily large numbers (using linked lists)

toDigits :: Int -> [Int]
toDigits n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigits (n `div` 10)

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (x : xs) =
  if isDigit x
    then x + (10 * fromDigits xs)
    else error ("The element " ++ show x ++ " is not a digit!")
  where
    isDigit x = 0 <= x && x < 10

-- Function assumes only valid inputs passes (i.e. digits 0-9)
addCarry :: Int -> Int -> Int -> (Int, Int) -- (carry, ans)
addCarry a b c = (n `div` 10, n `mod` 10) where n = a + b + c

addDigits :: [Int] -> [Int] -> [Int]
addDigits = addDigits' 0
  where
    addDigits' 0 [] [] = []
    addDigits' 1 [] [] = [1]
    addDigits' old_c (x : xs) [] =
      let (c, a) = addCarry old_c x 0 in a : addDigits' c xs []
    addDigits' old_c [] (y : ys) =
      let (c, a) = addCarry old_c 0 y in a : addDigits' c [] ys
    addDigits' old_c (x : xs) (y : ys) =
      let (c, a) = addCarry old_c x y in a : addDigits' c xs ys

add m n = fromDigits $ addDigits (toDigits m) (toDigits n)