module Interp where

import Parser
import Declare
import Prelude hiding (LT, GT, EQ)


-- | Question 1
--
-- >>> unary Not (BoolV True)
-- false
--
-- >>> unary Neg (IntV 3)
-- -3
--
-- >>> binary Add (IntV 2) (IntV 3)
-- 5

unary :: UnaryOp -> Value -> Value
unary Neg (IntV n)  = IntV (-n)
unary Neg b         = error ("Type Error: Expected Int, got: " ++ show b)
unary Not (BoolV b) = BoolV (not b)
unary Not n         = error ("Type Error: Expected Int, got: " ++ show n)


binary :: BinaryOp -> Value -> Value -> Value
binary Add (IntV m) (IntV n) = IntV (m + n)
binary Sub (IntV m) (IntV n) = IntV (m - n)
binary Mult (IntV m) (IntV n) = IntV (m * n)
binary Div (IntV m) (IntV n)
  | n == 0 = error "Division by Zero!"
  | otherwise = IntV (m `div` n)
binary GT (IntV m) (IntV n) = BoolV(m > n)
binary LT (IntV m) (IntV n) = BoolV(m < n)
binary LE (IntV m) (IntV n) = BoolV(m <= n)
binary GE (IntV m) (IntV n) = BoolV(m >= n)
binary EQ  a b = BoolV(a == b)
binary And (BoolV b) (BoolV c) = BoolV (b && c)
binary Or (BoolV b) (BoolV c) = BoolV (b || c)
binary _ _ _ = error "Type Error!"



type Binding = (String, Value)
type Env = [Binding]


-- | Question 2
--
-- >>> calc "1 + 2"
-- 3
--
-- >>> calc "if (true) 1; else 3"
-- 1
--
-- >>> calc "var x = 5; if (x > 0) x; else x * x"
-- 5

-- data Exp
--   = Literal Value
--   | Unary UnaryOp Exp
--   | Binary BinaryOp Exp Exp
--   | If Exp Exp Exp
--   | Var String
--   | Decl String Exp Exp

evaluate :: Exp -> Value
evaluate e = eval e []  -- starts with an empty environment
  where eval :: Exp -> Env -> Value
        eval (Literal a) [] = a
        eval (Unary op exp) [] = unary op (evaluate exp)
        eval (Binary op e1 e2) [] = binary op (evaluate e1) (evaluate e2)
        eval (If p e1 e2) [] = case evaluate p  of
          BoolV a -> if a then evaluate e1 else evaluate e2
          _       -> error ("Type Error: Expected Bool, got: "  ++ show p)
        eval (Var x) [] = error ("Undefined Variable" ++ x)
        eval (Decl x v b) [] = eval b [(x, evaluate b)]

calc :: String -> Value
calc = evaluate . parseExpr
