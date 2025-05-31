module Interp where

import Parser
import Declare
import Prelude hiding (LT, GT, EQ, (!?))
import Data.List (union, delete, elemIndex)

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n


-- | Free variables
--
-- >>> testq1 "1 + 2"
-- []
--
-- >>> testq1 "x * x"
-- ["x"]
--
-- >>> testq1 "var x = 3; x * y * z"
-- ["y","z"]
--
-- >>> testq1 "var x = y; var y = 3; x + y"
-- ["y"]

fv :: Exp -> [String]
fv (Literal _ )       = []
fv (Unary _ exp )     = fv  exp
fv (Binary _ e1 e2 )  = fv e1 `union` fv e2
fv (If p b1 b2)       = fv p `union` fv b1 `union` fv b2
fv (Var v)            = [v]
fv (Decl v a body)    = fv a `union` delete v (fv body)

testq1 :: String -> [String]
testq1 s = fv (parseExpr s) 


-- | Closed expressions
--
-- >>> testq2 "1 + 2"
-- True
--
-- >>> testq2 "x * x"
-- False
--
-- >>> testq2 "var x = y; var y = 3; x + y"
-- False
--
-- >>> testq2 "var x = 3; var y = 3; x + y"
-- True

closed :: Exp -> Bool
closed = null . fv

testq2 :: String -> Bool
testq2 s = closed (parseExpr s) 


type Binding = (String, Value)
type Env = [Binding]

-- | Evaluating source
--
-- >>> calc "1 + 2"
-- 3
--
-- >>> calc "if (true) 1; else 3"
-- 1
--
-- >>> calc "var x = 5; if (x > 0) x; else x * x"
-- 5

evaluate :: Exp -> Value
evaluate e = eval e [] -- starts with an empty environment
  where
    eval :: Exp -> Env -> Value
    eval (Literal n) _ = n
    eval (Unary op ex) env =
      unary op (eval ex env)
    eval (Binary op e1 e2) env =
      binary op (eval e1 env) (eval e2 env)
    eval (If e1 e2 e3) env =
      let BoolV test = eval e1 env
      in if test
           then eval e2 env
           else eval e3 env
    eval (Var v) env =
      case lookup v env of
        Just x  -> x
        Nothing -> error "Not found"
    eval (Decl v a b) env =
      let a' = eval a env
          env' = (v, a') : env
      in eval b env'

calc :: String -> Value
calc = evaluate . parseExpr

-- |
-- >>> unary Not (BoolV True)
-- false
--
-- >>> unary Neg (IntV 3)
-- -3
--
-- >>> binary Add (IntV 2) (IntV 3)
-- 5

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)  

binary :: BinaryOp -> Value -> Value -> Value
binary Add (IntV a) (IntV b)   = IntV (a + b)
binary Sub (IntV a) (IntV b)   = IntV (a - b)
binary Mult (IntV a) (IntV b)  = IntV (a * b)
binary Div (IntV a) (IntV b)   = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or (BoolV a) (BoolV b)  = BoolV (a || b)
binary GT (IntV a) (IntV b)    = BoolV (a > b)
binary LT (IntV a) (IntV b)    = BoolV (a < b)
binary LE (IntV a) (IntV b)    = BoolV (a <= b)
binary GE (IntV a) (IntV b)    = BoolV (a >= b)
binary EQ a b                  = BoolV (a == b)


-- | Translation

translate :: Exp -> TExp
translate source = convert source [] -- starts with an empty environment
  where
    convert :: Exp -> [String] -> TExp
    convert (Literal val) _ = TLit val
    convert (Unary op exp) env = TUnary op (convert exp env)
    convert (Binary op e1 e2) env = TBinary op (convert e1 env) (convert e2 env)
    convert (If p e1 e2) env = TIf (convert p env) (convert e1 env) (convert e2 env)
    convert (Var v) env = case elemIndex v env of
      Nothing -> error "Panic"
      Just n -> TVar n
    convert (Decl v a b) env = TDecl (convert a env)  (convert b (v : env))



-- | Evaluating target
--
-- >>> tcalc "1 + 2"
-- 3
--
-- >>> tcalc "if (true) 1; else 3"
-- 1
--
-- >>> tcalc "var x = 5; if (x > 0) x; else x * x"
-- 5
--
-- >>> tcalc "var x = 5; var y = x + 3; y + 2"
-- 10
--
-- >>> tcalc "var x = 5; var y = 6; var z = 4; y + x + z"
-- 15

tevaluate :: TExp -> Value
tevaluate e = teval e [] -- starts with an empty environment
  where
    teval :: TExp -> [Value] -> Value
    teval (TLit n) _ = n
    teval (TUnary op ex) env =
      unary op (teval ex env)
    teval (TBinary op e1 e2) env =
      binary op (teval e1 env) (teval e2 env)
    teval (TIf e1 e2 e3) env =
      let BoolV test = teval e1 env
       in if test
            then teval e2 env
            else teval e3 env
    teval (TVar n) env = env !! n
    teval (TDecl a b) env = teval b (teval a env : env)
                
tcalc :: String -> Value
tcalc = tevaluate . translate . parseExpr


data Type = TInt | TBool
          deriving (Eq, Show)

type TEnv = [Type]

-- | Type checking
--
-- >>> testq5 "1"
-- Just TInt
--
-- >>> testq5 "false"
-- Just TBool
--
-- >>> testq5 "1*false"
-- Nothing
--
-- >>> testq5 "var x = 5; if (x > 0) x; else x * x"
-- Just TInt
--
-- >>> testq5 "var x = false; var y = 3; x + y"
-- Nothing
--
-- >>> testq5 "var x = 1; var y = true; y"
-- Just TBool

tcheck :: TExp -> TEnv -> Maybe Type
tcheck (TLit (IntV _)) _ = Just TInt
tcheck (TLit (BoolV _)) _ = Just TBool
tcheck (TUnary op e) env = do
  t <- tcheck e env
  tunary op t
-- tcheck (TBinary op e1 e2) env = do
--   t1 <- 

testq5 :: String -> Maybe Type
testq5 = flip tcheck [] . translate . parseExpr

-- |
-- >>> tunary Neg TInt
-- Just TInt
--
-- >>> tbinary Add TInt TBool
-- Nothing

tunary :: UnaryOp -> Type -> Maybe Type
tunary Neg TInt = Just TInt
tunary Not TBool = Just TBool
tunary _ _ = Nothing

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary Add TInt  TInt  = Just TInt
tbinary Sub TInt  TInt  = Just TInt
tbinary Mult TInt  TInt  = Just TInt
tbinary Div TInt  TInt  = Just TInt
tbinary And TBool TBool = Just TBool
tbinary Or  TBool TBool = Just TBool
tbinary LT  TInt  TInt  = Just TBool
tbinary LE  TInt  TInt  = Just TBool
tbinary GE  TInt  TInt  = Just TBool
tbinary GT  TInt  TInt  = Just TBool
tbinary EQ  t1    t2    | t1 == t2 = Just TBool
tbinary _ _ _ = Nothing
