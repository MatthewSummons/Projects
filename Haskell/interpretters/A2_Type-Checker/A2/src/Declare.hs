module Declare where

import Data.Maybe (fromJust)
import Prelude hiding (LT, GT, EQ)
import Data.List (intercalate)

type Label = String

data BinaryOp = Add | Sub | Mult | Div
              | GT  | LT  | LE   | GE  | EQ
              | And | Or
              deriving Eq

data UnaryOp = Neg | Not
             deriving Eq

data Value = IntV Int
           | BoolV Bool
           | RecordV [(Label, Value)] -- { l1 = v1, l2 = v2, ... }
           deriving Eq

mapBp sep = map $ \(l, v) -> l ++ sep ++ show v

instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"
  show (RecordV lvs) = "{ " ++ items ++ " }" where
    items = intercalate ", " [l ++ " = " ++ show v | (l, v) <- lvs]

data Type = TInt | TBool
          | TArray Type -- [T]
          | TRecord [(Label, Type)] -- { l1: T1, l2: T2, ... }
          deriving Eq

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TArray t) = "[" ++ show t ++ "]"
  show (TRecord lvs) = "{ " ++ items ++ " }" where
    items = intercalate ", " [label ++ ": " ++ show val | (label, val) <- lvs]


-- Adding top-level functions

data Program = Program FunEnv Exp

type FunEnv = [(String, Function)]

data Function = Function [(String, Type)] Exp

data Exp = Lit Value
         | Unary UnaryOp Exp
         | Bin BinaryOp Exp Exp
         | If Exp Exp Exp
         | Var String
         | Decl String Exp Exp
         | Call String [Exp]
         | Array [Exp] -- [e1, e2, ...]
         | Index Exp Exp -- e1 !! (e2)
         | Record [(Label, Exp)] -- { l1 = e1, l2 = e2, ... }
         | Proj Exp Label -- e.l

prog1 :: Program
prog1 =
  Program [ ("absolute",
            Function [("x",TInt)]
                      (If (Bin GT
                              (Var "x")
                              (Lit (IntV 0)))
                          (Var "x")
                          (Unary Neg (Var "x"))))
          , ("max",
            Function [("x",TInt), ("y",TInt)]
                      (If (Bin GT
                              (Var "x")
                              (Var "y"))
                          (Var "x")
                          (Var "y")))
          ] (Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)])

prog2 :: Program
prog2 =
  Program [ ("max",
            Function [("x",TInt), ("y",TInt)]
                      (If (Bin GT
                              (Var "x")
                              (Var "y"))
                          (Var "x")
                          (Var "y")))
          , ("absolute",
            Function [("x",TInt)]
                      (Call "max" [Var "x", Unary Neg (Var "x")]))
          ] (Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)])


-- | Pretty-printing programs
--
-- Examples:
--
-- >>> show prog1
-- "function absolute(x: Int) {\n  if (x > 0) x; else -x\n}\nfunction max(x: Int, y: Int) {\n  if (x > y) x; else y\n}\nmax(absolute(-5), 4)"
--
-- >>> show prog2
-- "function max(x: Int, y: Int) {\n  if (x > y) x; else y\n}\nfunction absolute(x: Int) {\n  max(x, -x)\n}\nmax(absolute(-5), 4)"

instance Show Program where
  show (Program fenv e) = intercalate "\n" (map showFun fenv) ++ "\n" ++ show e

-- | Pretty-printing functions
--
-- Examples:
--
-- >>> showFun ("foo", Function [("x",TInt), ("y",TInt)] (Bin Add (Var "x") (Var "y")))
-- "function foo(x: Int, y: Int) {\n  x + y\n}"

showFun :: (String, Function) -> String
showFun (funcName, Function argList exp) =
  let
    showArgList = intercalate ", " $ mapBp ": " argList
  in
    "function " ++ funcName ++ "(" ++ showArgList ++ ") {\n  " ++ show exp ++ "\n}"


instance Show Exp where
  show = showExp 0

showExp :: Int -> Exp -> String
showExp _ (Call f args) = f ++ "(" ++ intercalate ", " (map show args) ++ ")"
showExp _ (Lit i) = show i
showExp _ (Var x) = x
showExp level (Decl x a b) =
  if level > 0
    then paren result
    else result
  where
    result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (If a b c) =
  if level > 0
    then paren result
    else result
  where
    result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c
showExp level (Array items) =
  if level > 0
    then paren result
    else result
  where
    result = "[" ++ intercalate ", " [showExp 0 item | item <- items] ++ "]"
showExp level (Index e1 e2) =
  if level > 0
    then paren result
    else result
  where
    result = showExp 0 e1 ++ " !! " ++ paren (showExp 0 e2)
showExp level (Record les) =
  if level > 0
    then paren result
    else result
  where
    result = "{ " ++ intercalate ", " [l ++ " = " ++ showExp 0 e | (l, e) <- les] ++ " }"
showExp level (Proj e l) =
  if level > 0
    then paren result
    else result
  where
    result = showExp 0 e ++ "." ++ l
showExp _ (Unary Neg a) = "-" ++ showExp 99 a
showExp _ (Unary Not a) = "!" ++ showExp 99 a
showExp level (Bin op a b) =
  showBin level (fromJust (lookup op levels)) a (fromJust (lookup op names)) b
  where
    levels =
      [ (Or, 1)
      , (And, 2)
      , (GT, 3)
      , (LT, 3)
      , (LE, 3)
      , (GE, 3)
      , (EQ, 3)
      , (Add, 4)
      , (Sub, 4)
      , (Mult, 5)
      , (Div, 5)
      ]
    names =
      [ (Or, "||")
      , (And, "&&")
      , (GT, ">")
      , (LT, "<")
      , (LE, "<=")
      , (GE, ">=")
      , (EQ, "==")
      , (Add, "+")
      , (Sub, "-")
      , (Mult, "*")
      , (Div, "/")
      ]

showBin :: Int -> Int -> Exp -> String -> Exp -> String
showBin outer inner a op b =
  if inner < outer
    then paren result
    else result
  where
    result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b

paren :: String -> String
paren x = "(" ++ x ++ ")"

unwrap (Right r) = r