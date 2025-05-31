module Interp where

import Declare
import Prelude hiding (LT, GT, EQ)
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
binary LT   (IntV a)  (IntV b)  = BoolV (a < b)
binary LE   (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE   (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT   (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ   a         b         = BoolV (a == b)
binary _ _ _ = undefined

updateEnv :: Ident -> Either ClosureExp Value -> Env -> Env
updateEnv _ _ [] = []
updateEnv name val ((n, _) : rest) | n == name = (n, val) : rest
updateEnv name val (pair : rest) = pair : updateEnv name val rest

-- Call-by-need evaluation
evaluate :: Exp -> Env -> (Value, Env)
evaluate (Lit v)            env = (v, env)
evaluate (Unary op e)       env = (unary op v, env')
    where (v, env') = evaluate e env
evaluate (Bin op e1 e2)     env = (binary op v1 v2, env'')
    where
        (v1, env')  = evaluate e1 env
        (v2, env'') = evaluate e2 env
evaluate (If p t f) env
    | pv        = evaluate t env'
    | otherwise = evaluate f env'
    where (BoolV pv, env') = evaluate p env
evaluate (Var x) env =
    case lookup x env of
        Just (Left (CExp e env')) -> (v, updateEnv x (Right v) env)
            where (v, _) = evaluate e env'
        Just (Right v) -> (v, env)
        Nothing -> error ("Variable " ++ show x ++ " is undefined" ++ show env)
evaluate (Decl x _ e body)  env = evaluate body newEnv
    where newEnv = ((x, Left (CExp e newEnv)) : env)
evaluate (Call e1 e2)       env = 
    case evaluate e1 env of
        (ClosureV (x, _) body clsEnv, env') -> (res, env')
            where (res, _ ) = evaluate body ((x, Left (CExp e2 env')) : clsEnv)
        _ -> error "Expected function during function call"
evaluate (Fun (x, t) body)  env = (ClosureV (x, t) body env, env)
evaluate (MultDecl xs body) env = evaluate body extEnv
    where extEnv = foldr ( \(name, _, expr) acc -> (name, Left (CExp expr extEnv)) : acc ) env xs

execute :: Exp -> Value
execute e = fst (evaluate e [])


-- | Test progs
--
-- >>> execute prog1
-- 10
--
-- >>> execute prog2
-- 11
--
-- >>> execute prog3
-- 8
--
-- >>> execute prog4
-- 20
--
-- >>> execute prog5
-- 1
--
-- >>> execute prog6
-- 120
--



prog7 :: Exp
prog7 = parseExpr "var x : Int = 0; (var x : Int = 1 + 1; x) + x"


-- | Test prog7
-- 
-- >>> execute prog7
-- 2

prog8 :: Exp
prog8 = parseExpr "vars even: Int -> Bool = function(x: Int) { if (x == 0) true; else odd (x-1) } and odd: Int -> Bool = function (x: Int) {  if (x == 0) false;  else even (x-1) }; even(4)"