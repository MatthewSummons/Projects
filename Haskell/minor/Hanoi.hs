module Hanoi where

-- Example Usage
-- Move three pegs from peg A to peg B using peg C
-- hanoi 3 "A" "B" "C"

type Peg = String
type Move = (Peg, Peg)

{-  The recursive calls correspond directly to the steps shown 
    1. move 𝑛 − 1 discs from A to C using B as temporary storage.
    2. move the top disc from A to B.
    3. move 𝑛 − 1 discs from C to B using A as temporary storage. -}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi discs fromPeg toPeg storePeg = step1 ++ step2 ++ step3 where
    step1 = hanoi (discs - 1) fromPeg storePeg toPeg
    step2 = [(fromPeg, toPeg)]
    step3 = hanoi (discs - 1) storePeg toPeg fromPeg
