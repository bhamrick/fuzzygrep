module NFA where

import Control.Monad
import Data.Ord
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

data Symbol = Chr Char | Start | End deriving (Eq, Show, Ord)

-- This will be an NFA that is limited to either single letter or wildcard transitions
-- Additionally, each non-accepting state will have an integral ID that determines equality
-- and must be set by the caller
data NFAState = Accept
              | Letter Int Symbol [NFAState]
              | Wildcard Int [NFAState]

state_id :: NFAState -> Int
state_id Accept = -1
state_id (Letter i _ _) = i
state_id (Wildcard i _) = i

instance Show NFAState where
    show Accept = "Accept"
    show (Letter i c next) = "Letter " ++ (show i) ++ " " ++ (show c) ++ " " ++ (show . map state_id $ next)
    show (Wildcard i next) = "Wildcard" ++ (show i) ++ " " ++ (show . map state_id $ next)

instance Eq NFAState where
    (==) Accept Accept = True
    (==) Accept _ = False
    (==) _ Accept = False
    (==) x y = (state_id x) == (state_id y)

instance Ord NFAState where
    compare Accept Accept = EQ
    compare Accept _ = LT
    compare _ Accept = GT
    compare x y = compare (state_id x) (state_id y)

-- Construction functions
-- These generally take the states to move to if the subexpression matches
-- and return the start state(s) of the subexpression

-- Concatenation is simply the composition operator
-- (.) :: ([NFAState] -> [NFAState]) -> ([NFAState] -> [NFAState]) -> [NFAState] -> [NFAState]

letter :: Int -> Symbol -> [NFAState] -> [NFAState]
letter i c next = return $! Letter i c next

wildcard :: Int -> [NFAState] -> [NFAState]
wildcard i next = return $! Wildcard i next

zeroOrOne :: ([NFAState] -> [NFAState]) -> [NFAState] -> [NFAState]
zeroOrOne builder next = next ++ (builder next)

zeroOrMore :: ([NFAState] -> [NFAState]) -> [NFAState] -> [NFAState]
zeroOrMore builder next = let x = (builder next ++ x) in next ++ x

fork :: [[NFAState] -> [NFAState]] -> [NFAState] -> [NFAState]
fork paths next = join $ map ($ next) paths

oneOrMore :: ([NFAState] -> [NFAState]) -> [NFAState] -> [NFAState]
oneOrMore builder next = let x = (builder (next ++ x)) in x

-- Running an NFA
singleStep :: Symbol -> NFAState -> S.Set NFAState
singleStep c Accept = S.singleton Accept
singleStep c (Letter _ c0 next) = if c == c0 then S.fromList next else S.empty
singleStep _ (Wildcard _ next) = S.fromList next

-- >>= for sets
setBind :: (Ord a, Ord b) => S.Set a -> (a -> S.Set b) -> S.Set b
setBind xs f = S.foldl' S.union S.empty (S.map f xs)

step :: Symbol -> S.Set NFAState -> S.Set NFAState
step c states = states `setBind` (singleStep c)

runNFA :: [Symbol] -> S.Set NFAState -> S.Set NFAState
runNFA [] states = states
runNFA (c:cs) states = runNFA cs (step c states)

accepted :: S.Set NFAState -> Bool
accepted = S.member Accept
