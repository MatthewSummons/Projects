module StackMachine where

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

data Frame = FUnary UnaryOp
           | FBin BinaryOp (Either Value Exp)
           | FIf Exp Exp
           | FCall1 Exp
           | FCall2 Value
           | FDecl String Exp
           | FEnv Env  -- Env changed here

type Stack = [Frame]

data State = Eval Stack Exp
           | Return Stack Value

getEnv :: Stack -> Env
getEnv [] = []
getEnv (FEnv env : k) = env
getEnv (_ : k) = getEnv k

step :: State -> State
step (Eval k (Lit v)) = Return k v
step (Eval k (Unary op e)) = Eval (FUnary op : k) e
step (Return (FUnary op : k) v) = Return k (unary op v)
step (Eval k (Bin op e1 e2)) = Eval (FBin op (Right e2) : k) e1
step (Return (FBin op (Right e2) : k) v1) = Eval (FBin op (Left v1) : k) e2
step (Return (FBin op (Left v1) : k) v2) = Return k (binary op v1 v2)
step (Eval k (If e1 e2 e3)) = Eval (FIf e2 e3 : k) e1
step (Return (FIf e2 e3 : k) (BoolV True)) = Eval k e2
step (Return (FIf e2 e3 : k) (BoolV False)) = Eval k e3
step (Eval k (Fun (para, _) body)) = Return k (ClosureV para body (getEnv k))
step (Eval k (Call e1 e2)) = Eval (FCall1 e2 : k) e1
step (Return (FCall1 e2 : k) v1) = Eval (FCall2 v1 : k) e2
step (Return (FCall2 (ClosureV x e cenv) : k) v2) = Eval (FEnv ((x, Right v2) : cenv) : k) e -- changed
step (Eval k (Decl x _ e1 e2)) = Eval (FEnv newEnv : FDecl x e2 : k) e1
  where newEnv = (x, Left (FExp e1 newEnv)): getEnv k
step (Return (FDecl x e2 : k) v1) = Eval (FEnv ((x, Right v1) : getEnv k) : k) e2 -- changed
step (Eval k (Var x)) = case lookup x (getEnv k) of
  Nothing -> error "Undefined variable"
  Just (Right v) -> Return k v
  Just (Left (FExp e env)) -> Eval (FEnv env : k) e

step (Return (FEnv _ : k) v) = Return k v
step _ = error "stuck!"

execute :: Exp -> Value
execute e = go (Eval [] e)
  where go :: State -> Value
        go (Return [] v) = v
        go s = go (step s)
