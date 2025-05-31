module Interp where

import Parser
import Declare
import Prelude hiding (Either(..))


-- | Evaluation function
--
-- Examples:
--
-- >>> evaluate (Neg (Num 3))
-- -3
--
-- >>> evaluate (Power (Num 2) (Num 3))
-- 8
evaluate :: Exp -> Int
evaluate (Num n) = n
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Sub a b) = evaluate a - evaluate b
evaluate (Mult a b) = evaluate a * evaluate b
evaluate (Div a b) = evaluate a `div` evaluate b
evaluate (Power a n) = evaluate a ^ evaluate n 
evaluate (Neg a) = negate (evaluate a)



-- | A simple calculator
--
-- Examples:
--
-- >>> calc "1 + 8 * 2"
-- 17
--
-- >>> calc "2 * (8 + -6) ^ 3"
-- 16
--
-- >>> calc "3 * 4 - 1"
-- 11
calc :: String -> Int
calc = evaluate . parseExpr


-- | Error handling

data Either a b = Left a | Right b deriving Show

safeHead :: [a] -> Either String a
safeHead []    = Left "can't access the head of an empty list"
safeHead (x:_) = Right x


-- | Evaluation function, revisited
--
-- Examples:
--
-- >>> evaluate2 (Add (Sub (Num 3) (Num 2)) (Mult (Num 2) (Num 3)))
-- Right 7
--
-- >>> evaluate2 (Div (Num 2) (Num 0))
-- Left "Divided by zero: 0"
--
-- >>> evaluate2 (Power (Num 2) (Num (-3)))
-- Left "To the power of a negative number: -3"
evaluate2 :: Exp -> Either String Int
evaluate2 (Num n) = Right n
evaluate2 (Add a b) =
    case evaluate2 a of
      Right a -> case evaluate2 b of
        Right b -> Right (a + b)
        Left m -> Left m
      Left m -> Left m
evaluate2 (Sub a b) =
    case evaluate2 a of
      Right a -> case evaluate2 b of
        Right b -> Right (a - b)
        Left m -> Left m
      Left m -> Left m
evaluate2 (Mult a b) =
    case evaluate2 a of
      Right a -> case evaluate2 b of
        Right b -> Right (a * b)
        Left m -> Left m
      Left m -> Left m
evaluate2 (Div a b) =
    case evaluate2 a of
      Right a -> case evaluate2 b of
        Right 0 -> Left "Zero Division Error"
        Right b -> Right (a `div` b)
        Left m -> Left m
      Left m -> Left m
evaluate2 (Power a n) =
    case evaluate2 a of
      Right a -> case evaluate2 n of
        Right n -> if n >= 0 then Right (a ^ n) else Left "Negative Exponents Error"
        Left m -> Left m
      Left m -> Left m
evaluate2 (Neg a) =
    case evaluate2 a of
      Right a -> Right (negate a)
      Left m -> Left m


calc2 :: String -> String
calc2 cmd = 
    case evaluate2 (parseExpr cmd) of
        Right x -> show x
        Left y -> y
