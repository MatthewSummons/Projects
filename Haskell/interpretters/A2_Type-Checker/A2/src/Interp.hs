module Interp where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Maybe (fromJust)
import Parser

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)
unary _ _ = undefined

binary :: BinaryOp -> Value -> Value -> Value
binary Add  (IntV a)  (IntV b)  = IntV (a + b)
binary Sub  (IntV a)  (IntV b)  = IntV (a - b)
binary Mult (IntV a)  (IntV b)  = IntV (a * b)
binary Div  (IntV a)  (IntV b)  = IntV (a `div` b)
binary And  (BoolV a) (BoolV b) = BoolV (a && b)
binary Or   (BoolV a) (BoolV b) = BoolV (a || b)
binary GT   (IntV a)  (IntV b)  = BoolV (a > b)
binary LT   (IntV a)  (IntV b)  = BoolV (a < b)
binary LE   (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE   (IntV a)  (IntV b)  = BoolV (a >= b)
binary EQ   a         b         = BoolV (a == b)
binary _ _ _ = undefined


type Binding = (String, Value)
type Env = [Binding]

execute :: Program -> Value
execute (Program funEnv main) = evaluate main [] funEnv

-- |
-- >>> evaluate ((unwrap . parseExpr) "[1,3,5]") [] []
-- { 0 = 1, 1 = 3, 2 = 5 }
--
-- >>> evaluate ((unwrap . parseExpr) "var r = {a=[[1,2,3]],b=true}; r.a!!0!!(3-2)") [] []
-- 2
--
-- >>> execute ((unwrap . parseProg) "function fact(x: Int) { if (x == 0) 1; else x * fact(x - 1) } fact(5)")
-- 120

evaluate :: Exp -> Env -> FunEnv -> Value
evaluate e env fenv = eval e env
  where
    eval :: Exp -> Env -> Value
    eval (Lit n) _ = n
    eval (Unary op ex) env = unary op (eval ex env)
    eval (Bin op e1 e2) env = binary op (eval e1 env) (eval e2 env)
    eval (If e1 e2 e3) env =
      let BoolV test = eval e1 env
      in if test
         then eval e2 env
         else eval e3 env
    eval (Var v) env = case lookup v env of
      Just v -> v
      Nothing -> error ("Variable " ++ v ++ " is undefined!")
    eval (Decl v a b) env =
      let a' = eval a env
          env' = (v, a') : env
      in eval b env'
    eval (Record les) env = 
      RecordV [(label, eval exp env) | (label, exp) <- les]
    eval (Proj e l) env = case eval e env  of
      RecordV lvs -> case lookup l lvs of
        Just val -> val
        Nothing  -> error ("Index (field) " ++ l ++ " out of bounds")
      _ -> error "Can only fetch from a record/array!"
    eval (Array items) env = 
      eval (Record [(show i, v) | (i, v) <- zip [0..] items]) env
    eval (Index e1 e2) env = case eval e2 env of
      (IntV n) -> eval (Proj e1 (show n)) env
      _      -> error "Index must be an integer!"
    eval (Call fname args) env = case lookup fname fenv of
      Just (Function params body) -> 
        eval body newEnv where
          paramNames = [param | (param, _) <- params]
          arguments  = [eval arg env | arg <- args]
          newEnv   = (zip paramNames arguments) ++ env
      Nothing -> error ("Function " ++ fname ++ " is undefined!")

-- | Function substitution
--
-- Examples:
--
-- >>> fsubst ("absolute", Function [("x",TInt)] (If (Bin GT (Var "x") (Lit (IntV 0))) (Var "x") (Unary Neg (Var "x")))) (Call "absolute" [Lit (IntV (-5))])
-- var x = -5; if (x > 0) x; else -x
--
-- >>> fsubst ("absolute", Function [("x",TInt)] (If (Bin GT (Var "x") (Lit (IntV 0))) (Var "x") (Unary Neg (Var "x")))) (Call "absolute" [Call "absolute" [Lit (IntV (-5))]])
-- var x = var x = -5; if (x > 0) x; else -x; if (x > 0) x; else -x
--
-- (Although the pretty-printing seems weird, the expression above is valid.)

fsubst :: (String, Function) -> Exp -> Exp
fsubst (f, Function xs body) = fsub where 
  fsub :: Exp -> Exp
  fsub (Lit v) = Lit v
  fsub (Unary op e)   = Unary op (fsub e)
  fsub (Bin op e1 e2) = Bin op (fsub e1) (fsub e2)
  fsub (If p t f)     = If (fsub p) (fsub t) (fsub f)
  fsub (Var x)        = Var x
  fsub (Decl x e b)   = Decl x (fsub e) (fsub b)
  fsub (Array items)  = Array (map fsub items)
  fsub (Index e1 e2)  = Index (fsub e1) (fsub e2)
  fsub (Record les)   = Record [(l, fsub e) | (l, e) <- les]
  fsub (Proj e l)     = Proj (fsub e) l
  fsub (Call f' args)
    | f == f'   = nestDecl (zip xs args) body
    | otherwise = Call f' (map fsub args)
    where
      nestDecl :: [((String, Type), Exp)] -> Exp -> Exp
      nestDecl []               body = fsub body
      nestDecl (((p, _), a):as) body = nestDecl as (Decl p (fsub a) body)


-- | Execution with function substitution
--
-- Examples:
--
-- >>> execute' prog1
-- 5
--
-- >>> execute' prog2
-- 5
--
-- >>> execute' ((unwrap . parseProg) "function f() {1} function g() {f()} g()")
-- 1
--
-- >>> execute' ((unwrap . parseProg) "function f(a:Int) {1} f(f(1))")
-- 1
--
-- >>> execute' ((unwrap . parseProg) "function f(a:Int,b:Int) {a+b} f(3,4)")
-- 7

-- execute' :: Program -> Value
execute' :: Program -> Value
execute' (Program [] main) = evaluate' main []
execute' (Program (f:fs) main) = execute' (Program newFEnv (fsubst f main))
  where 
    newFEnv = [(fname, Function args (fsubst f body)) | (fname, Function args body) <- fs]


evaluate' :: Exp -> Env -> Value
evaluate' (Lit n) _ = n
evaluate' (Unary op e) env = unary op (evaluate' e env)
evaluate' (Bin op e1 e2) env = binary op (evaluate' e1 env) (evaluate' e2 env)
evaluate' (If e1 e2 e3) env =
  let BoolV test = evaluate' e1 env
  in if test
     then evaluate' e2 env
     else evaluate' e3 env
evaluate' (Var v) env = case lookup v env of
  Just v -> v
  Nothing -> error ("Variable " ++ v ++ " is undefined!")
evaluate' (Decl v a b) env =
  let a' = evaluate' a env
      env' = (v, a') : env
  in evaluate' b env'
evaluate' (Record les) env = RecordV [(label, evaluate' exp env) | (label, exp) <- les]
evaluate' (Proj e l) env = case evaluate' e env  of
  RecordV lvs -> case lookup l lvs of
    Just val -> val
    Nothing  -> error ("Index (field) " ++ l ++ " out of bounds")
  _ -> error "Can only fetch from a record/array!"
evaluate' (Array items) env = evaluate' (Record [(show i, v) | (i, v) <- zip [0..] items]) env
evaluate' (Index e1 e2) env = case evaluate' e2 env of
  (IntV n) -> evaluate' (Proj e1 (show n)) env
  _ -> error "Index must be an integer!"
evaluate' e _ = error ("You are in trouble" ++ (show e))