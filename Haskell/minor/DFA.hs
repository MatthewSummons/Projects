module DFA where

-- An implementation of Discrete Finite Automata
-- See Reference A2 for the specification

import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

type State = String

data DFA = DFA
  { states :: Set State,
    alphabet :: Set Char,
    transition :: Map (State, Char) State,
    initialState :: State,
    acceptingState :: Set State
  }
  deriving (Show, Eq)

-- [DFA.Construct]
dfa :: [State] -> [Char] -> [(State, Char, State)] -> State -> [State] -> Maybe DFA
dfa q sigma delta q0 f =
  if checkTransitions transitions && (q0 `S.member` qSet) && (fSet `S.isSubsetOf` qSet)
    then
      Just
        DFA
          { states = qSet,
            alphabet = alphaSet,
            transition = M.fromList transitions,
            initialState = q0,
            acceptingState = fSet
          }
    else Nothing
  where
    (qSet, alphaSet, fSet) = (S.fromList q, S.fromList sigma, S.fromList f)
    transitions =
      [((s, a), s') | (s, a, s') <- delta, s `S.member` qSet, s' `S.member` qSet, a `S.member` alphaSet]

{- Check if the transition function/map is well-defined i.e.
  if (s, a) -> s' and (s, a) -> s'' then s' == s''.  If this is
  not the case then the map is not well-defined.      -}
checkTransitions :: [((State, Char), State)] -> Bool
checkTransitions [] = True
checkTransitions (((s, a), s') : xs) = case lookup (s, a) xs of
  Just s'' -> (s' == s'') && checkTransitions xs
  Nothing -> checkTransitions xs

-- [DFA.Step]
step :: DFA -> State -> Char -> Maybe State
step m s a = (s, a) `M.lookup` transition m

-- [DFA.Accept]
accept :: DFA -> String -> Bool
accept m [] = initialState m `S.member` acceptingState m
accept m (a : as) = case step m (initialState m) a of
  Just s' -> accept m {initialState = s'} as
  Nothing -> False

-- [DFA.Search]
search :: DFA -> Maybe String
search m = dfs (S.fromList []) [initialState m] ""
  where
    -- Perform DFS on the DFA to determine if an accepting state is reachable
    dfs :: Set State -> [State] -> String -> Maybe String
    dfs _ [] _ = Nothing
    dfs visited (q : qs) path
      | q `S.member` acceptingState m = Just path
      | q `S.member` visited = dfs visited qs path
      | otherwise =
          let newVisited = S.insert q visited
              nextStates = [(s', a) | a <- S.toList (alphabet m), Just s' <- [step m q a]]
              tryNext [] = dfs newVisited qs path
              tryNext ((s', a) : rest) =
                case dfs newVisited (s' : qs) (path ++ [a]) of
                  Just p -> Just p
                  Nothing -> tryNext rest
           in tryNext nextStates
