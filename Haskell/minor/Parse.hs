module Parse where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Map (fromList, insert, lookup, toList, union)
import Data.String

-- Basic parsers
data Parser a = P (String -> [(a, String)])

-- Functor instance for Parser
instance Functor Parser where
  fmap f p = P (\inp -> [(f v, out) | (v, out) <- parse p inp])

-- Applicative instance for Parser
instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pf <*> px = P (\inp -> [(f x, out') | (f, out) <- parse pf inp, (x, out') <- parse px out])

-- The monad of parsers (for the do-notation)
instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  (>>=) p f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

-- parsing functions
failure :: Parser a
failure = P (const [])

-- alternative parser: tries to apply p, and if that fails applies q
(+++) :: Parser a -> Parser a -> Parser a
p +++ q =
  P
    ( \inp -> case parse p inp of
        [] -> parse q inp
        r -> r
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  (if p x then return x else failure)

item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

-- parse a single character
char :: Char -> Parser Char
char c = sat (c ==)

-- Applies a Parser 0 or more times
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- Apply the parser at least one time
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

digit :: Parser Char
digit = sat isDigit

csvSep = do
  char ','
  many (char ' ')

text c = many1 $ sat (c /=)

stoi :: String -> Int
stoi = read

csvParse = do
  id <- many1 digit
  csvSep
  num <- many1 digit
  csvSep
  fstName <- text ','
  csvSep
  lstName <- many1 item
  many (char ' ')
  return (stoi id, (stoi num, fstName, lstName))

parseFile fname = do
  text <- readFile fname
  return [(id, vs) | [((id, vs), _)] <- map (parse csvParse) (lines text)]

-- concatF :: Map -> [String, (String, String, String)] -> Map
-- concatF :: Data.Map String (String, String, String)
concatF m [] = m
concatF m ((id, (n, fst, lst)) : kvs) = case Data.Map.lookup id m of
  Nothing -> insert id (n, fst, lst) m
  Just (oldID, fst', lst') -> insert id (n + oldID, fst', lst') m `union` concatF m kvs

main = do
  l1 <- parseFile "file1.txt"
  l2 <- parseFile "file2.txt"
  let m1 = fromList l1
   in writeFile "concatenated.txt" (unlines $ map pp (toList (concatF m1 l2)))
  where
    pp (id, (n, fst, lst)) = show id ++ "," ++ show n ++ "," ++ show fst ++ "," ++ show lst