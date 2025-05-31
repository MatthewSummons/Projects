{-# LANGUAGE FlexibleInstances #-}

module Monads where

import Prelude hiding (Maybe(..), Either(..), List(..), concat, map)

type Loc = (Int, Int) -- (x, y)

data Maybe a = Nothing
             | Just a 
             deriving Show

data Either a b = Left a
                | Right b
                deriving Show

data List a = Nil
            | Cons a (List a)
            deriving Show

singleton :: a -> List a
singleton = (`Cons` Nil)

--------
-- UP --
--------

up :: Loc -> Loc
up (x, y) = (x, y+1) 

up1 :: Loc -> Maybe Loc
up1 = Just . up

up2 :: Loc -> List Loc
up2 = singleton . up

up3 :: Loc -> Either String Loc
up3 = Right . up

----------
-- DOWN --
----------

down :: Loc -> Loc
down (x, y) = (x, y-1)

down1 :: Loc -> Maybe Loc
down1 loc = if snd loc == 0
            then Nothing
            else Just $ down loc

down2 :: Loc -> List Loc
down2 loc = if snd loc == 0
            then Nil 
            else singleton $ down loc

down3 :: Loc -> Either String Loc
down3 loc = if snd loc == 0
            then Left $ "Cannot move " ++ show loc ++ " down"
            else Right $ down loc

----------
-- LEFT --
----------

left :: Loc -> Loc
left (x, y) = (x-1, y)

left1 :: Loc -> Maybe Loc
left1 loc = if fst loc == 0
            then Nothing
            else Just $ left loc

left2 :: Loc -> List Loc
left2 loc = if fst loc == 0
            then Nil 
            else singleton $ left loc

left3 :: Loc -> Either String Loc
left3 loc = if fst loc == 0
            then Left $ "Cannot move " ++ show loc ++ " left"
            else Right $ left loc

--------------
-- Examples --
--------------

update :: (Int -> Int) -> (Int -> Int) -> Loc -> Loc
update f g (x, y) | f x >= 0 && g y >= 0 = (f x, g y) 
                  | otherwise            = (x, y)

ex1 :: Loc -> Maybe Loc
ex1 loc = case left1 loc of
            Nothing   -> Nothing
            Just loc' -> case down1 loc' of
                           Nothing    -> Nothing
                           Just loc'' -> Just $ update (+1) (+1) loc''

ex2 :: Loc -> List Loc
ex2 loc = case left2 loc of
            Nil         -> Nil 
            Cons loc' _ -> case down2 loc' of
                             Nil          -> Nil 
                             Cons loc'' _ -> singleton $ update (+1) (+1) loc''

ex3 :: Loc -> Either String Loc
ex3 loc = case left3 loc of
            Left err   -> Left err 
            Right loc' -> case down3 loc' of
                            Left err'   -> Left err' 
                            Right loc'' -> Right $ update (+1) (+1) loc''

---------------------
-- Defining Monads --
---------------------

instance Monad Maybe where
  return a    = Just a
  ma >>= a2mb = case ma of Nothing -> Nothing
                           Just a  -> a2mb a

instance Monad (Either String) where
  return a    = Right a
  ma >>= a2mb = case ma of Left str -> Left str
                           Right a  -> a2mb a

map :: (a -> b) -> List a -> List b
map f Nil         = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

append :: List a -> List a -> List a
append Nil         l = l
append (Cons x xs) l = Cons x (append xs l)

concat :: List (List a) -> List a
concat Nil         = Nil 
concat (Cons x xs) = append x (concat xs) 

instance Monad List where
  return a    = singleton a
  ma >>= a2mb = concat (map a2mb ma)

------------------
-- Using Monads --
------------------

ex1monad :: Loc -> Maybe Loc
ex1monad loc = left1 loc  >>= \loc'  ->
               down1 loc' >>= \loc'' ->
               return $ update (+1) (+1) loc''

ex1do :: Loc -> Maybe Loc
ex1do loc = do loc'  <- left1 loc 
               loc'' <- down1 loc' 
               return $ update (+1) (+1) loc''

ex2do :: Loc -> List Loc 
ex2do loc = do
  loc'  <- left2 loc
  loc'' <- down2 loc'
  return $ update (+1) (+1) loc''


ex3do :: Loc -> Either String Loc
ex3do loc = do
  loc'  <- left3 loc
  loc'' <- down3 loc'
  return $ update (+1) (+1) loc''

----------------------------
-- Parametrized by Monads --
----------------------------

exM :: Monad m => (Loc -> m Loc) -> (Loc -> m Loc) -> (Loc -> m Loc) -> Loc -> m Loc
exM up down left loc = do
  loc'  <- left loc
  loc'' <- down loc'
  return $ update (+1) (+1) loc''

exMaybe :: Loc -> Maybe Loc
exMaybe = exM up1 down1 left1

exList :: Loc -> List Loc
exList = exM up2 down2 left2

exEither :: Loc -> Either String Loc
exEither = exM up3 down3 left3

---------------------------------
-- Instances to make GHC happy --
---------------------------------

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure a = Just a 
  Just f <*> Just a = Just (f a)
  _      <*> _      = Nothing

instance Functor (Either String) where
  fmap _ (Left str) = Left str 
  fmap f (Right a)  = Right (f a)

instance Applicative (Either String) where
  pure a = Right a 
  Right f  <*> Right a  = Right (f a)
  Left err <*> _        = Left err
  _        <*> Left err = Left err

instance Functor List where
  fmap = map

instance Applicative List where
  pure a = Cons a Nil
  Cons f fs <*> Cons a as = Cons (f a) (fs <*> as)
  _         <*> _         = Nil
