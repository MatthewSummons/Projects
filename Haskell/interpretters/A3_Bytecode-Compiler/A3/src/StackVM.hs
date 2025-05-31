module StackVM where

import DeclareVM
import Data.Maybe (fromJust)


-- | Compiler
--
-- Examples:
--
-- >>> sCompile prog0
-- [IPush 1,IStore "x",IPush 2,ILoad "x",IBin +,IStoreDead]
--
-- >>> sCompile prog4
-- [IPushFun "f" [IPushFun "x" [ILoad "f",ILoad "x",ICall,IReturn],IReturn],IStore "apply",IPushFun "x" [ILoad "x",IPush 2,IBin *,IReturn],IStore "double",ILoad "apply",ILoad "double",ICall,IPush 10,ICall,IStoreDead,IStoreDead]
--
-- >>> sCompile prog6
-- [IPushFun "x" [IPushFun "y" [ILoad "x",ILoad "y",IBin -,IPush 0,IBin >=,IIf [ILoad "x"] [ILoad "y"],IReturn],IReturn],IStore "max",ILoad "max",IPush 9,ICall,IPush 10,ICall,IStoreDead]
--
-- >>> sCompile prog8
-- [IPush 1,IStore "x",IPush 2,IStoreDead,ILoad "x",IBin +]

sCompile :: Exp -> SProg
sCompile (Lit v)              = [IPush v]
sCompile (Unary op e)         = sCompile e ++ [IUnary op]
sCompile (Bin op e1 e2)       = sCompile e1 ++ sCompile e2 ++ [IBin op]
sCompile (If p e1 e2)         = sCompile p ++ [IIf (sCompile e1) (sCompile e2)]
sCompile (Var x)              = [ILoad x]
sCompile (Decl x _ e1 e2)     = sCompile e1 ++ (IStore x : sCompile e2  ++ [IStoreDead])
sCompile (Call e1 e2)         = sCompile e1 ++ sCompile e2 ++ [ICall]
sCompile (Fun (para, _) body) = [IPushFun para ((sCompile body) ++ [IReturn])]

-- auxiliary functions

load :: Ident -> Env -> Value
load x env = fromJust $ lookup x env

store :: Ident -> Value -> Env -> Env
store x v e = (x, v) : e

removeLatest :: Env -> Env
removeLatest = tail

-- | Executor
--
-- Examples:
--
-- >>> sExecute [IPush (IntV 1),IStore "x",IPush (IntV 2),ILoad "x",IBin Add,IStoreDead]
-- 3
--
-- >>> sExecute [IPushFun "f" [IPushFun "x" [ILoad "f",ILoad "x",ICall,IReturn],IReturn],IStore "apply",IPushFun "x" [ILoad "x",IPush (IntV 2),IBin Mult,IReturn],IStore "double",ILoad "apply",ILoad "double",ICall,IPush (IntV 10),ICall,IStoreDead,IStoreDead]
-- 20
--
-- >>> sExecute [IPushFun "x" [IPushFun "y" [ILoad "x",ILoad "y",IBin Sub,IPush (IntV 0),IBin GE,IIf [ILoad "x"] [ILoad "y"],IReturn],IReturn],IStore "max",ILoad "max",IPush (IntV 9),ICall,IPush (IntV 10),ICall,IStoreDead]
-- 10

sStep :: State -> State
sStep ((inst : prog), stack, fr@(env : frames)) = step inst where
  step :: SInst -> State
  step (IPush v) = (prog, (v : stack), fr)
  step (IPushFun param body) = (prog, (ClosureV param body env) : stack, fr)
  step (ILoad x) = (prog, (load x env) : stack, fr)
  step (IStore x) = (prog, tail stack, newEnv : frames)
    where newEnv = store x (head stack) env
  step IStoreDead = (prog, stack, ((removeLatest env) : frames))
  step (IUnary op) = case stack of
    (v : vs) -> (prog, (unary op v) : vs, fr)
    _        -> error "Empty Stack on IUnary"
  step (IBin op) = case stack of
    (a : b : vs) -> (prog, (binary op b a) : vs, fr)
    _        -> error "Not enough arguments on stack for IBin"
  step (IIf n1 n2) = case stack of
    (BoolV b : vs) -> if b 
      then (n1 ++ prog, vs, fr)
      else (n2 ++ prog, vs, fr)
    _             -> error "No boolean on stack for Iff instruction"
  step ICall = case stack of
    (v : cls@(ClosureV arg body clsEnv) : vs) ->
      let (_, [result], _) = executeBody (body, [], (store arg v clsEnv) : frames)
        in (prog, result : vs, fr)
    _ -> error ("Callee function not found in ICall")
    where 
      executeBody s@([], [v], _) = s
      executeBody s = executeBody (sStep s)
  step IReturn = (prog, stack, frames)
sStep (p, st, env) = error $ "You are in trouble when executing " ++ show p ++ ": Stack = " ++ show st ++ ", Environment = " ++ show env



-- | Whole Process
--
-- Examples:
--
-- >>> run prog1
-- 10
--
-- >>> run prog2
-- 11
--
-- >>> run prog3
-- 8
--
-- >>> run prog5
-- 3
--
-- >>> run prog7
-- 5
--
-- >>> run prog9
-- 3

sExecute :: SProg -> Value
sExecute p = go (p, [], [[]])
  where go :: State -> Value
        go ([], [v], [[]]) = v
        go s = go (sStep s)

run :: Exp -> Value
run = sExecute . sCompile