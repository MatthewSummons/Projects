module Interp where

import Declare
import Data.Maybe (fromJust)
import Prelude hiding (LT, GT, EQ)

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
binary LT   (IntV a)  (IntV b)  = BoolV (a < b)
binary LE   (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE   (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT   (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ   a         b         = BoolV (a == b)
binary _ _ _ = undefined

evaluate :: Exp -> Env -> Value
evaluate (Lit v) _ = v
evaluate (Unary op a) env = unary op (evaluate a env)
evaluate (Bin op a b) env = binary op (evaluate a env) (evaluate b env)
evaluate (If a b c) env =
  let (BoolV test) = evaluate a env
  in if test
     then evaluate b env
     else evaluate c env
evaluate (Var x) env = fromJust (lookup x env)
evaluate (Decl x _ e body) env =
  -- when evaluating `e`, use `newEnv` instead of `env`
  let newEnv = (x, evaluate e newEnv) : env
  in evaluate body newEnv
evaluate (Call fun arg) env =
  let ClosureV (x, _) body env' = evaluate fun env
      v = evaluate arg env
  in evaluate body ((x, v) : env')
evaluate (Fun (x, t) body) env = ClosureV (x, t) body env

execute :: Exp -> Value
execute e = evaluate e []


-- Stack machine

data Frame = FUnary UnaryOp
           | FBin BinaryOp (Either Value Exp)
           | FIf Exp Exp
           | FCall1 Exp
           | FCall2 Value
           | FDecl (String, Exp)
           | FEnv Env

type Stack = [Frame]

data State = Eval Stack Exp
           | Return Stack Value

getEnv :: Stack -> Env
getEnv [] = []
getEnv (FEnv env : k) = env
getEnv (_ : k) = getEnv k


step :: State -> State
step (Eval k (Lit v)) = Return k v
step (Eval k (UnaryOp op e)) = Eval (FUnary op : k) e
step (Eval k (UnaryOp op e)) = Eval (FUnary op : k) e
step (Return (FUnary op : k) v) = Return k (unary op e)
step (Eval k (BinaryOp op e1 e2)) = Eval (FBin op (Right e2)) e1
-- step (Eval ((FBin op Left v1):k) v1) = Eval (FBin op (Left e2))

execute' :: Exp -> Value
execute' e = go (Eval [] e)
  where go :: State -> Value
        go (Return [] v) = v
        go s = go (step s)
