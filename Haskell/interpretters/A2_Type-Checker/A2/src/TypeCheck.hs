module TypeCheck where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Maybe (isJust, fromJust)
import Parser

type TEnv = [(String,Type)]

type TFunEnv = [(String, (TEnv, Type))]

tunary :: UnaryOp -> Type -> Maybe Type
tunary Neg TInt = Just TInt
tunary Not TBool = Just TBool
tunary _ _ = Nothing

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary Add  TInt  TInt  = Just TInt
tbinary Sub  TInt  TInt  = Just TInt
tbinary Mult TInt  TInt  = Just TInt
tbinary Div  TInt  TInt  = Just TInt
tbinary And  TBool TBool = Just TBool
tbinary Or   TBool TBool = Just TBool
tbinary LT   TInt  TInt  = Just TBool
tbinary LE   TInt  TInt  = Just TBool
tbinary GE   TInt  TInt  = Just TBool
tbinary GT   TInt  TInt  = Just TBool
tbinary EQ   t1    t2    | t1 == t2 = Just TBool
tbinary _ _ _ = Nothing


-- | Type checking function definitions
--
-- Examples:
--
-- >>> :{
--   checkFunEnv [ ("foo", Function [("x",TInt), ("y",TInt)] (Bin Add (Var "x") (Var "y")))
--               , ("bar", Function [("x",TInt)] (Call "foo" [Var "x", Var "x"]))
--               ]
-- :}
-- Just [("bar",([("x",Int)],Int)),("foo",([("x",Int),("y",Int)],Int))]
--
-- >>> :{
--   checkFunEnv [ ("foo", Function [("x",TInt), ("y",TInt)] (Bin Add (Var "x") (Var "y")))
--               , ("err", Function [("x",TInt)] (Call "foo" [Var "x"]))
--               ]
-- :}
-- Nothing
--
-- >>> checkFunEnv [ ("err", Function [("x",TInt)] (Call "foo" [Var "x", Var "x"])) ]
-- Nothing
--
-- >>> checkFunEnv [ ("err", Function [("x",TBool)] (Bin Add (Var "x") (Var "x"))) ]
-- Nothing

checkFunEnv :: FunEnv -> Maybe TFunEnv
checkFunEnv fds = checkFunEnv' fds [] where
  checkFunEnv' :: FunEnv -> TFunEnv -> Maybe TFunEnv
  checkFunEnv' [] tfEnv = Just tfEnv
  checkFunEnv' ((f, Function argTypes body):fs) tfEnv = 
    case tcheck body argTypes tfEnv of
      Just returnType -> 
        case checkFunEnv' fs ((f, (argTypes, returnType)):tfEnv) of
          Just validTFEnv -> Just validTFEnv
          Nothing -> Nothing
      Nothing -> Nothing where

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x ==) xs

-- |
-- >>> tcheck ((unwrap . parseExpr) "{a=[[1,2,3]],b=true}") [] []
-- Just { a: [[Int]], b: Bool }
--
-- >>> tcheck ((unwrap . parseExpr) "var r = {a=[[1,2,3]],b=true}; r.a!!0!!(3-2)") [] []
-- Just Int
--
-- >>> tcheck ((unwrap . parseExpr) "[{a=1,b=true},{a=2,b=false}]") [] []
-- Just [{ a: Int, b: Bool }]
--
-- >>> tcheck ((unwrap . parseExpr) "[1,false]") [] []
-- Nothing
--
-- >>> tcheck ((unwrap . parseExpr) "[]!!true") [] []
-- Nothing
--
-- >>> tcheck ((unwrap . parseExpr) "{c=2}.d") [] []
-- Nothing

tcheck :: Exp -> TEnv -> TFunEnv -> Maybe Type
tcheck (Call name args) tenv fenv = case lookup name fenv of
  -- Type Check the passed arguments & compare with expected types
  Just (paramTypes, outType) -> if length argTypes == length args 
    then if argTypes == [t | (_, t) <- paramTypes]
        then Just outType else Nothing 
    else Nothing 
  Nothing -> Nothing 
  where
    argTypes = [fromJust t | e <- args, t <- [tcheck e tenv fenv], isJust t]
tcheck (Lit v) _ _ =
  case v of
    IntV _  -> Just TInt
    BoolV _ -> Just TBool
tcheck (Unary op e) tenv fenv =
  case tcheck e tenv fenv of
    Just t  -> tunary op t
    Nothing -> Nothing
tcheck (Bin op e1 e2) tenv fenv =
  case (tcheck e1 tenv fenv, tcheck e2 tenv fenv) of
    (Just t1, Just t2) -> tbinary op t1 t2
    _                  -> Nothing
tcheck (If e1 e2 e3) tenv fenv =
  case tcheck e1 tenv fenv of
    Just TBool ->
      case (tcheck e2 tenv fenv, tcheck e3 tenv fenv) of
        (Just t2, Just t3) | t2 == t3 -> Just t2
        _ -> Nothing
    _ -> Nothing
tcheck (Var v) tenv _ = lookup v tenv
tcheck (Decl v e1 e2) tenv fenv =
  case tcheck e1 tenv fenv of
    Just t  -> tcheck e2 ((v, t) : tenv) fenv
    Nothing -> Nothing
tcheck (Record les) tenv fenv = let 
    fieldTypes = [(l, t) | (l, e) <- les, Just t <- [tcheck e tenv fenv]] 
  in if length fieldTypes == length les
    then Just (TRecord fieldTypes)
    else Nothing
tcheck (Proj e l) tenv fenv = case tcheck e tenv fenv of
  Just (TRecord lts) -> lookup l lts
  _                  -> Nothing
tcheck (Array items) tenv fenv
  | validArray = Just (TArray (arrType elemTypes))
  | otherwise  = Nothing
  where 
    elemTypes = [t | e <- items, Just t <- [tcheck e tenv fenv]]
    validArray = allSame elemTypes && length items == length elemTypes 
    arrType []     = TInt
    arrType (t:ts) = t
tcheck (Index e1 e2) tenv fenv = 
  case (tcheck e1 tenv fenv, tcheck e2 tenv fenv) of 
    (Just (TArray t), Just TInt) -> Just t
    _                            -> Nothing

-- | Type checking function definitions
--
-- Examples:
--
-- >>> checkProgram prog1
-- True
--
-- >>> checkProgram prog2
-- True
--
-- >>> checkProgram (Program [] (Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)]))
-- False
--
-- >>> checkProgram ((unwrap . parseProg) "function f(a:Int,b:Int) {a+b} f(3,4)")
-- True
--
-- >>> checkProgram ((unwrap . parseProg) "function f(a:Int,b:Int) {a+b} f(3+true,4)")
-- False
--
-- >>> checkProgram ((unwrap . parseProg) "function f(a:Int,b:Int) {a+b} f(3)")
-- False

checkProgram :: Program -> Bool
checkProgram (Program fds main) = case checkFunEnv fds of
  Just fenv -> isJust (tcheck main [] fenv)
  Nothing -> False