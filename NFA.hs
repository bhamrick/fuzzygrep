module NFA where

import Control.Monad
import Data.Ord
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Intable as I

data Symbol = Chr Char | Start | End deriving (Eq, Show, Ord)

-- This will be an NFA that is limited to either single letter or wildcard transitions
-- Additionally, each non-accepting state will have an integral ID that determines equality
-- and must be set by the caller
data NFAState = Accept
              | Letter Int Symbol (I.Set NFAState)
              | Wildcard Int (I.Set NFAState)

state_id :: NFAState -> Int
state_id Accept = -1
state_id (Letter i _ _) = i
state_id (Wildcard i _) = i

instance I.Intable NFAState where
    toInt = state_id

instance Show NFAState where
    show Accept = "Accept"
    show (Letter i c next) = "Letter " ++ (show i) ++ " " ++ (show c) ++ " " ++ (show . I.map state_id $ next)
    show (Wildcard i next) = "Wildcard" ++ (show i) ++ " " ++ (show . I.map state_id $ next)

instance Eq NFAState where
    (==) Accept Accept = True
    (==) Accept _ = False
    (==) _ Accept = False
    (==) x y = (state_id x) == (state_id y)

instance Ord NFAState where
--    compare Accept Accept = EQ
--    compare Accept _ = LT
--    compare _ Accept = GT
    compare x y = compare (state_id x) (state_id y)

-- Construction functions
-- These generally take the states to move to if the subexpression matches
-- and return the start state(s) of the subexpression

-- Concatenation is simply the composition operator (.)

letter :: Int -> Symbol -> I.Set NFAState -> I.Set NFAState
letter i c next = I.singleton $! Letter i c next

wildcard :: Int -> I.Set NFAState -> I.Set NFAState
wildcard i next = I.singleton $! Wildcard i next

zeroOrOne :: (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
zeroOrOne builder next = I.union next (builder next)

zeroOrMore :: (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
zeroOrMore builder next = let x = (builder $ I.union next x) in I.union next x

fork :: [I.Set NFAState -> I.Set NFAState] -> I.Set NFAState -> I.Set NFAState
fork paths next = I.unions (map ($ next) paths)

oneOrMore :: (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
oneOrMore builder next = let x = (builder (I.union next x)) in x

-- Running an NFA
singleStep :: Symbol -> NFAState -> I.Set NFAState
singleStep c Accept = I.singleton Accept
singleStep c (Letter _ c0 next) = if c == c0 then next else I.empty
singleStep _ (Wildcard _ next) = next

step :: Symbol -> I.Set NFAState -> I.Set NFAState
step c states = states `I.setBind` (singleStep c)

runNFA :: [Symbol] -> I.Set NFAState -> I.Set NFAState
runNFA [] states = states
runNFA (c:cs) states = runNFA cs (step c states)

accepted :: I.Set NFAState -> Bool
accepted = I.member Accept
