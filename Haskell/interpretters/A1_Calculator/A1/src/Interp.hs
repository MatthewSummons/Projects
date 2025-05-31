module Interp where

import Parser
import Declare


-- | Evaluation function
--
-- Examples:
--
-- >>> evaluate e1
-- Right 7
--
-- >>> evaluate e2
-- Right (-4)
--
-- >>> evaluate e3
-- Right 9
--
-- >>> evaluate e4
-- Right 2
--
-- >>> evaluate e5
-- Right 1
--
-- >>> evaluate (Add (Sub (Num 3) (Num 2)) (Mult (Num 2) (Num 3)))
-- Right 7
--
-- >>> evaluate (Div (Num 2) (Num 0))
-- Left "Divided by zero: 0"
--
-- >>> evaluate (Power (Num 2) (Num (-3)))
-- Left "To the power of a negative number: -3"
--
-- >>> evaluate (Fact (Sub (Num 2) (Num 3)))
-- Left "Factorial of a negative number: (2 - 3)"
--
-- >>> evaluate (Mod (Num 2) (Num 0))
-- Left "Divided by zero: 0"
--
-- >>> evaluate (Fact (Mod (Num 2) (Num 3)))
-- Right 2
evaluate :: Exp -> Either String Int
evaluate (Num n) = Right n
evaluate (Add a b) =
  case evaluate a of
    Left msg -> Left msg
    Right a' ->
      case evaluate b of
        Left msg -> Left msg
        Right b' -> Right (a' + b')
evaluate (Sub a b) =
  case evaluate a of
    Left msg -> Left msg
    Right a' ->
      case evaluate b of
        Left msg -> Left msg
        Right b' -> Right (a' - b')
evaluate (Mult a b) =
  case evaluate a of
    Left msg -> Left msg
    Right a' ->
      case evaluate b of
        Left msg -> Left msg
        Right b' -> Right (a' * b')
evaluate (Div a b) =
  case evaluate a of
    Left msg -> Left msg
    Right a' ->
      case evaluate b of
        Left msg -> Left msg
        Right 0  -> Left ("Divided by zero: " ++ show b) 
        Right b' -> Right (a' `div` b')
evaluate (Power a b) =
  case evaluate a of
    Left msg -> Left msg
    Right a' ->
      case evaluate b of
        Left msg -> Left msg
        Right b' | b' < 0    -> Left ("To the power of a negative number: " ++ show b)
                 | otherwise -> Right (a' ^ b')
evaluate (Neg a) =
  case evaluate a of
    Left msg -> Left msg
    Right a' -> Right (negate a')
evaluate (Fact a) =
  case evaluate a of
    Left msg -> Left msg
    Right a'  | a' < 0    -> Left ("Factorial of a negative number: " ++ show a) 
              | otherwise -> Right (product [1..a'])
evaluate (Mod a b) =
  case evaluate a of
    Left msg -> Left msg
    Right a' -> 
      case evaluate b of
        Left msg -> Left msg
        Right b' | b' == 0   -> Left ("Divided by zero: " ++ show b)
                 | otherwise -> Right (a' `mod` b')
evaluate e@(Var name) = Left ("Variable not found: " ++  show e)


-- | Calculator with Error Handling
--
-- Examples:
--
-- >>> calc [] "1 + 8 * 2"
-- Right 17
--
-- >>> calc [] "2 * (8 + -6) ^ 3"
-- Right 16
--
-- >>> calc [] "5! * 2 - 3 + 6 "
-- Right 243
--
-- >> calc [] "- (6! + 2) % 7 * 9 "
-- Right (-9)
--
-- >>> calc env1 "x + (z * x) ^ 2"
-- Left "Variable not found: z"
--
-- >>> calc env2 "2 ^ (y - z) + 12"
-- Left "To the power of a negative number: (5 - 6)"
--
-- >>> calc env3 "z ! + (y * x) ^ 2"
-- Right 1120
calc :: Env -> String -> Either String Int
calc env = evaluate . substs env . parseExpr


-- | Monadic binding operation
--
-- Examples:
-- >>> andThen (Right 3) (\n -> Right (n + 1))
-- Right 4
--
-- >>> andThen (Left "bad things") (\n -> Right (n + 1))
-- Left "bad things"
andThen :: Either a b -> (b -> Either a c) -> Either a c
andThen (Left msg)  _ = Left msg
andThen (Right n) f = f n

-- | Taming Eithers
--

evaluate2 :: Exp -> Either String Int
evaluate2 (Num n)      = Right n
evaluate2 (Add a b)    = evaluate2 a `andThen` \a' -> evaluate2 b `andThen` \b' -> Right (a' + b')
evaluate2 (Sub a b)    = evaluate2 a `andThen` \a' -> evaluate2 b `andThen` \b' -> Right (a' - b')
evaluate2 (Mult a b)   = evaluate2 a `andThen` \a' -> evaluate2 b `andThen` \b' -> Right (a' * b')
evaluate2 (Neg a)      = evaluate2 a `andThen` \a' -> Right (-a')
evaluate2 e@(Var name) = Left ("Variable not found: " ++ show e)
evaluate2 (Div a b)    = evaluate2 b `andThen` \b' ->
  if b' == 0
    then Left ("Divided by zero: " ++ show b)
    else evaluate2 a `andThen` \a' -> Right (a' `div` b')
evaluate2 (Fact a) = evaluate2 a `andThen` \a' -> if a' < 0
  then Left ("Factorial of a negative number: " ++ show a) 
  else Right (product [1 .. a'])
evaluate2 (Mod a b) = evaluate2 b `andThen` \b' -> 
  if b' == 0 
    then Left ("Divided by zero: " ++ show b)
    else evaluate2 a `andThen` \a' -> Right (a' `mod` b')
evaluate2 (Power a b) = evaluate2 b `andThen` \b' ->
  if b' < 0
    then Left ("To the power of a negative number: " ++ show b)
    else evaluate2 a `andThen` \a' -> Right (a' ^ b')

-- | The neat calculator
--
-- Examples:
--
-- >>> calc2 [] "1 + 8 * 2"
-- Right 17
--
-- >>> calc2 [] "2 * (8 + -6) ^ 3"
-- Right 16
--
-- >>> calc2 [] "2 * (4 - 2) / (8 - 8)"
-- Left "Divided by zero: (8 - 8)"
--
-- >>> calc2 [] "2 * (4 - 2) ^ (6 - 8)"
-- Left "To the power of a negative number: (6 - 8)"
-- 
-- >>> calc2 [] "(8 + 4) % (6 - 6)"
-- Left "Divided by zero: (6 - 6)"
-- 
-- >>> calc2 [] "2 * (1 - 3)!"
-- Left "Factorial of a negative number: (1 - 3)"
--
-- >>> calc2 env1 "x + (y * x) ^ 2"
-- Right 404
--
-- >>> calc2 env2 "2 * (z - y - 1) ^ 3 / (z * y - 30)"
-- Left "Divided by zero: ((6 * 5) - 30)"
--
-- >>> calc2 env3 "xy + (y * x) ^ 2"
-- Left "Variable not found: xy"
calc2 :: Env -> String -> Either String Int
calc2 env = evaluate2 . substs env . parseExpr


-- | Eliminating zeros
--
-- Examples:
--
-- >>> elim0 (Mult (Num 3) (Num 0))
-- 0
--
-- >>> elim0 (Mult (Num 3) (Num 2))
-- (3 * 2)
--
-- >>> elim0 (Add (Num 4) (Neg (Mult (Num 3) (Num 0))))
-- 4
--
-- >>> elim0 (Add (Mod (Num 0) (Num 5)) (Fact (Num 0)))
-- 1
elim0 :: Exp -> Exp
elim0 (Num n) = Num n
elim0 (Add a b) = case (elim0 a, elim0 b) of
  (a', Num 0) -> a'
  (Num 0, b') -> b'
  (a', b')    -> Add a' b'
elim0 (Sub a b) = case (elim0 a, elim0 b) of
  (a', Num 0) -> a'
  (a', b')    -> Sub a' b'
elim0 (Mult a b) = case (elim0 a, elim0 b) of
  (_, Num 0)  -> Num 0
  (Num 0, _)  -> Num 0
  (a', b')    -> Mult a' b'
elim0 (Div a b) = case (elim0 a, elim0 b) of
  (Num 0, _)  -> Num 0
  (a', b')    -> Div a' b'
elim0 (Mod a b) = case (elim0 a, elim0 b) of
  (Num 0, _)  -> Num 0
  (a', b')    -> Mod a' b'
elim0 (Power a b) = case (elim0 a, elim0 b) of
  (_, Num 0)  -> Num 1
  (Num 0, _)  -> Num 0
  (a', b')    -> Power a' b'
elim0 (Neg a) = case elim0 a of
  Num 0       -> Num 0
  a'          -> Neg a'
elim0 (Fact a) = case elim0 a of
  Num 0       -> Num 1
  a'          -> Fact a'
elim exp = exp


calc3 :: String -> Either String Int
calc3 = evaluate2 . elim0 . parseExpr