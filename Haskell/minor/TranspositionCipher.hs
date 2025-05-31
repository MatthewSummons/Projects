module TranspositionCipher where

-- A simple transposition cipher

{- 
    Notice that in the transposition cipher we decompose the string into two lines.
    The second line consists of characters at indices 1, 4, 7, ... i.e. (i-1) ≡ 0 (mod 3), or
    equivalently,  i-1 is a multiple of 3, s.t. 0 ≤ i ≤ length (word). The first line consists of
    all other characters.
    To construct the cipher, we simply collect all characters in each line into lists that we concatenate
    together to get our final cipher.
-}
cipher :: String -> String
cipher xs = 
    [c | (c, i) <- zip xs [0 ..], (i - 1) `mod` 3 /= 0] ++ [c | (c, i) <- zip xs [0 ..], (i - 1) `mod` 3 == 0]

{-
    Recall the model of the cipher with the decomposition of a string into two lines. Define l_1 to be the
    length of the first line, and l_2 be that of the second line. 
    Then it is not hard to prove that l_1 is either 2l_2 - 1, 2l_2, or 2l_2 + 1. It is also clear that
    the total length of the cipher, n, is the sum of l_1 and l_2. Then, n = 3l_2 or 3l_2 ± 1. Then to 
    find the length of the second line, we use divMod to find the quotient and residue mod 3, based on 
    which we split the original string. When the residue is zero or one, we know that l1 = 2l_2 or
    2l_2 + 1 respectivly. In these cases, the quotient is exactly l_2. However, when l1 ≡ 2l_2 - 1 (mod 3),
    the quotient, q, is actually one unit below l_2. This is because 3l_2 - 1 = 3(l_2 - 1) + 2,
    resulting in q = l_2 - 1 as the quotient. To compute l_1 from here is simple. 
        l_1 = 2l_2 - 1 = l_2 = 2(q + 1) - 1 = 2q + 1.

    Once we have retrieved the two lines, we can construct the original text using the weave function.
    The weave function simply reads the two lines in the prescribed zig-zag manner to reconstruct the
    original text.
-}
decipher :: String -> String
decipher cs = case divMod (length cs) 3 of
    (k, 2) -> weave (splitAt ((2 * k) + 1) cs)
    (k, i) -> weave (splitAt ((2 * k) + i) cs)
    where
        weave (x1:x2:xs, y:ys) = x1 : y : x2 : weave (xs, ys)
        weave (x:xs,     y:ys) = [x,y]
        weave (xs,       []  ) = xs

