module Declare where

import Data.List (intercalate, nub, lookup)

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Var String
         | Decl String Exp Exp
         | DeclareMulti [(String, Exp)] Exp


type Binding = (String, Int)
type Env = [Binding]

e1 :: Exp
e1 = Decl "x" (Num 3) (Mult (Var "x") (Num 3))

e2 :: Exp
e2 = Decl "x" (Add (Num 3) (Num 4)) (Var "x")

e3 :: Exp
e3 = Add (Var "x") (Mult (Num 4) (Num 5))

e4 :: Exp
e4 = Decl "y" e3 (Div (Var "x") (Var "y"))


instance Show Exp where
  show = showExp


-- | Pretty printer
--
-- Examples:
--
-- >>> e1
-- var x = 3; (x * 3)
--
-- >>> e2
-- var x = (3 + 4); x
--
-- >>> e3
-- (x + (4 * 5))
--
-- >>> e4
-- var y = (x + (4 * 5)); (x / y)
--
-- >>> e8
-- var a = 2, b = 7; (var m = (5 * a), n = (b - 1); ((a * n) + (b / m)) + a)
showExp :: Exp -> String
showExp (Num n) = show n
showExp (Add e1 e2) = "(" ++ showExp e1 ++ " + " ++ showExp e2 ++ ")"
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ " - " ++ showExp e2 ++ ")"
showExp (Mult e1 e2) = "(" ++ showExp e1 ++ " x " ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ " ÷ " ++ showExp e2 ++ ")"
showExp (Var s) = show s
showExp (Decl s asgn body) = 
  "val " ++ show s ++ " = " ++ show asgn  ++ "; " ++ show body



-- | Renaming function
--
-- Examples:
--
-- >>> rename "x" "i" e3
-- (i + (4 * 5))
--
-- >>> rename "x" "i" e1
-- var x = 3; (x * 3)
--
-- >>> rename "x" "i" e4
-- var y = (i + (4 * 5)); (i / y)
-- rename :: String -> String -> Exp -> Exp
-- 

isKey :: Eq a => a -> [(a, b)] -> Bool
isKey x = any (\(s, _) -> s == x)

hasDuplicate :: Eq a => [(a, b)] -> Bool
hasDuplicate lst = str /= nub str
  where str = map fst lst


-- | Substitution function
--
-- Examples:
--
-- >>> subst ("x", 2) e1
-- var x = 3; (x * 3)
--
-- >>> subst ("a", 2) e10
-- var b = 8; var a = b, b = 2; (a + b)
subst :: Binding -> Exp -> Exp
subst (x, n) = sub where
  sub (Num i) = Num i
  sub (Add e1 e2) = Add (sub e1) (sub e2)
  sub (Sub e1 e2) = Sub (sub e1) (sub e2)
  sub (Mult e1 e2) = Mult (sub e1) (sub e2)
  sub (Div e1 e2) = Div (sub e1) (sub e2)
  sub (Var y) = if x == y then Num n else Var y
  sub (Decl name exp body)  =
    if name == x then   -- the variable is shadowed 
        Decl name (sub exp) body
    else  -- there is no shadowing
        Decl name (sub exp) (sub body)
  sub (DeclareMulti bs body) = error "TODO: Question 4"