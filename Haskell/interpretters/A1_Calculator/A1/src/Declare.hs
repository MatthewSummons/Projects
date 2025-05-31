module Declare where


data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Power Exp Exp
         | Neg Exp
         | Fact Exp
         | Mod Exp Exp
         | Var String
        deriving (Eq)

instance Show Exp where
  show = showExp

type Env = [(String, Int)]

e1 :: Exp
e1 = Add (Num 3) (Num 4)

e2 :: Exp
e2 = Add (Num 3) (Mult (Sub (Num 4) (Num 5)) (Num 7))

e3 :: Exp
e3 = Sub (Div (Add (Num 1) (Num 2)) (Num 3)) (Mult (Sub (Num 5) (Num 6)) (Num 8))

e4 :: Exp
e4 = Mod (Fact (Num 3)) (Num 4)

e5 :: Exp
e5 = Add (Fact (Num 0)) (Mod (Num 6) (Num 3))


e6 :: Exp
e6 = Add (Var "x") (Mult (Num 3) (Var "x"))

e7 :: Exp
e7 = Div (Var "y") (Sub (Num 3) (Var "x"))

e8 :: Exp
e8 = Add (Var "x") (Mult (Fact (Var "x")) (Var "y"))

e9 :: Exp
e9 = Mod (Var "x") (Num 2)

env1 :: Env
env1 = [("x", 4), ("y", 5)]

env2 :: Env
env2 = [("y", 5), ("z", 6)]

env3 :: Env
env3 = [("x", 4), ("y", 5), ("z", 6)]



-- Question 1
-- Please write down the corresponding Haskell definition for the following
-- expression:
--            Mult
--           /   \
--          /     \
--         Sub    Add
--        / |      | \
--       /  |      |  \
--      7  Power  Neg  Div
--         / \     |   /  \
--        4   3    6  3    1

e :: Exp
e = Mult 
  (Sub (Num 7) (Power (Num 4) (Num 3))) 
  (Add (Neg (Num 6)) (Div (Num 3) (Num 1)))

-- 
-- Question 2
-- please write down the evaluation result of `e` in the comment below
-- 
-- Answer: 171
-- 



-- | Pretty printing
--
-- Examples:
--
-- >>> e1
-- (3 + 4)
--
-- >>> e2
-- (3 + ((4 - 5) * 7))
--
-- >>> e3
-- (((1 + 2) / 3) - ((5 - 6) * 8))
--
-- >>> e4
-- ((3!) % 4)
--
-- >>> e5
-- ((0!) + (6 % 3))
showExp :: Exp -> String
showExp (Num n)           = show n
showExp (Add exp1 exp2)   = "(" ++ showExp exp1 ++ " + " ++ showExp exp2 ++ ")"
showExp (Sub exp1 exp2)   = "(" ++ showExp exp1 ++ " - " ++ showExp exp2 ++ ")"
showExp (Mult exp1 exp2)  = "(" ++ showExp exp1 ++ " * " ++ showExp exp2 ++ ")"
showExp (Div exp1 exp2)   = "(" ++ showExp exp1 ++ " / " ++ showExp exp2 ++ ")"
showExp (Power exp1 exp2) = "(" ++ showExp exp1 ++ " ^ " ++ showExp exp2 ++ ")"
showExp (Neg exp)         = "(-" ++ showExp exp ++ ")"
showExp (Fact exp)        = "(" ++ showExp exp ++ "!)"
showExp (Mod exp1 exp2)   = "(" ++ showExp exp1 ++ " % " ++ showExp exp2 ++ ")"
showExp (Var s)           = s




-- | Substitution
--
-- Examples:
--
-- >>> subst "x" 4 (Var "x")
-- 4
--
-- >>> subst "x" 4 e6
-- (4 + (3 * 4))
--
-- >>> subst "x" 4 e7
-- (y / (3 - 4))
--
-- >>> subst "xy" 4 e8
-- (x + ((x!) * y))
--
-- >>> subst "x" 0 e9
-- (0 % 2)
subst :: String -> Int -> Exp -> Exp
subst var val = sub where 
  sub (Num n) = Num n
  sub (Add e1 e2)   = Add (sub e1) (sub e2)
  sub (Sub e1 e2)   = Sub (sub e1) (sub e2)
  sub (Mult e1 e2)  = Mult (sub e1) (sub e2)
  sub (Div e1 e2)   = Div (sub e1) (sub e2)
  sub (Power e1 e2) = Power (sub e1) (sub e2)
  sub (Neg e)       = Neg (sub e)
  sub (Fact e)      = Fact (sub e)
  sub (Mod e1 e2)   = Mod (sub e1) (sub e2)
  sub (Var s) 
    | var == s      = Num val
    | otherwise     = Var s


-- | Multiple Substitution
--
-- Examples:
--
-- >>> substs env1 (Add (Var "x") (Var "y"))
-- (4 + 5)
--
-- >>> substs env1 e6
-- (4 + (3 * 4))
--
-- >>> substs env1 e7
-- (5 / (3 - 4))
--
-- >>> substs env2 e8
-- (x + ((x!) * 5))
--
-- >>> substs env3 e9
-- (4 % 2)
substs :: Env -> Exp -> Exp
substs [] exp = exp
substs ((var,val):bs) exp = substs bs (subst var val exp)