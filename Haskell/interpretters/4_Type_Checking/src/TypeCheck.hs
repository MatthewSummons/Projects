module TypeCheck where

import Declare
import Interp
import Parser
import Prelude hiding (LT, GT, EQ)


data Type = TInt | TBool
          deriving (Eq, Show)

type TEnv = [(String, Type)]


-- | Question 3
--
-- >>> tunary Neg TInt
-- Just TInt
--
-- >>> tbinary Add TInt TBool
-- Nothing

tunary :: UnaryOp -> Type -> Maybe Type
tunary Neg TInt     = Just TInt
tunary Not TBool    = Just TBool
tunary _ _          = Nothing

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary Add TInt TInt = Just TInt
tbinary Sub TInt TInt = Just TInt
tbinary Mult TInt TInt = Just TInt
tbinary Div TInt TInt = Just TInt
tbinary GT TInt TInt = Just TBool
tbinary LT TInt TInt = Just TBool
tbinary LE TInt TInt = Just TBool
tbinary GE TInt TInt = Just TBool
tbinary EQ a b = Just TBool
tbinary And TBool TBool = Just TBool
tbinary Or TBool TBool = Just TBool
tbinary _ _ _ = Nothing



-- | Question 4
--
-- >>> testq4 "1"
-- Just TInt
--
-- >>> testq4 "false"
-- Just TBool
--
-- >>> testq4 "1*false"
-- Nothing
--
-- >>> testq4 "var x = 5; if (x > 0) x; else x * x"
-- Just TInt
--
-- >>> testq4 "var x = y; var y = 3; x + y"
-- Nothing


tcheck :: Exp -> TEnv -> Maybe Type
tcheck (Literal (IntV n)) env  = Just TInt
tcheck (Literal (BoolV b)) env = Just TBool
tcheck (Unary op exp) env = case tcheck exp env of
    (Just t) -> tunary op t
    _        -> Nothing
tcheck (Binary op e1 e2) env = case (tcheck e1 env, tcheck e2 env) of
    (Just t1, Just t2) -> tbinary op t1 t2
    _        -> Nothing
tcheck (If p e1 e2) env = case tcheck p env of
    Just TBool -> case (tcheck e1 env, tcheck e2 env) of
        (Just t1, Just t2) -> if t1 == t2 then Just t1 else Nothing
        _ -> Nothing
    _          -> Nothing
tcheck (Var x) env = case lookup x env of 
    Just t -> Just t
    _ -> Nothing
tcheck (Decl x v b) env = case tcheck v env of
    (Just t) -> tcheck b [(x, t)]
    _        -> Nothing



testq4 :: String -> Maybe Type
testq4 e = tcheck (parseExpr e) []


-- | Question 5
--
-- >>> tcalc "3 == 3"
-- true
--
-- >>> tcalc "if (3 == 4) true; else false"
-- false
--
-- >>> tcalc "var x = 3; x + true"
-- *** Exception: You have a type error in your program!

tcalc :: String -> Value
tcalc s = case tcheck (parseExpr s) [] of
    Just t ->  evaluate (parseExpr s)
    _      -> error "Type Error!"

