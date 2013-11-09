module NFA where

import Control.Monad
import Data.Ord
import Data.List
import qualified Data.Set as S
import qualified Intable as I

data Symbol = Chr Char | Start | End deriving (Eq, Show, Ord)

isLetter :: Symbol -> Bool
isLetter (Chr c) = True
isLetter Start = False
isLetter End = False

-- This will be an NFA that is limited to either single letter or wildcard transitions
-- Additionally, each non-accepting state will have an integral ID that determines equality
-- and must be set by the caller. We also keep track of all possible next states
-- so that it is possible to explore the state graph
data NFAState = Accept
              | Transition Int (I.Set NFAState) Bool (Symbol -> I.Set NFAState)

state_id :: NFAState -> Int
state_id Accept = -1
state_id (Transition i _ _ _) = i

allowFuzz :: NFAState -> Bool
allowFuzz Accept = False
allowFuzz (Transition _ _ b _) = b

instance I.Intable NFAState where
    toInt = state_id

instance Show NFAState where
    show Accept = "Accept"
    show (Transition i _ _ _) = "Transition state " ++ (show i)

instance Eq NFAState where
    (==) Accept Accept = True
    (==) Accept _ = False
    (==) _ Accept = False
    (==) x y = (state_id x) == (state_id y)

instance Ord NFAState where
    compare x y = compare (state_id x) (state_id y)

-- Construction functions
-- These generally take the states to move to if the subexpression matches
-- and return the start state(s) of the subexpression

-- Concatenation is simply the composition operator (.)

letter :: Int -> Symbol -> I.Set NFAState -> I.Set NFAState
letter i c next = I.singleton $! Transition i next True (\x -> if x == c then next else I.empty)

letters :: Int -> [Symbol] -> I.Set NFAState -> I.Set NFAState
letters i cs next = I.singleton $! Transition i next True (\x -> if x `S.member` (S.fromList cs) then next else I.empty)

start :: Int -> I.Set NFAState -> I.Set NFAState
start i next = I.singleton $! Transition i next False (\x -> if x == Start then next else I.empty)

end :: Int -> I.Set NFAState -> I.Set NFAState
end i next = I.singleton $! Transition i next False (\x -> if x == End then next else I.empty)

notLetters :: Int -> [Symbol] -> I.Set NFAState -> I.Set NFAState
notLetters i cs next = I.singleton $! Transition i next True (\x -> if x `S.member` S.fromList cs then I.empty else next)

wildcard :: Int -> I.Set NFAState -> I.Set NFAState
wildcard i next = I.singleton $! Transition i next True (const next)

zeroOrOne :: (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
zeroOrOne builder next = I.union next (builder next)

zeroOrMore :: (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
zeroOrMore builder next = let x = (builder $ I.union next x) in I.union next x

fork :: [I.Set NFAState -> I.Set NFAState] -> I.Set NFAState -> I.Set NFAState
fork paths next = I.unions (map ($ next) paths)

parallel :: (I.Set NFAState -> I.Set NFAState) -> (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
parallel f g next = I.union (f next) (g next)

oneOrMore :: (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
oneOrMore builder next = let x = (builder (I.union next x)) in x

-- Running an NFA
singleStep :: Symbol -> NFAState -> I.Set NFAState
singleStep c Accept = I.singleton Accept
singleStep c (Transition _ _ _ f) = f c

step :: Symbol -> I.Set NFAState -> I.Set NFAState
step c states = states `I.setBind` (singleStep c)

runNFA :: [Symbol] -> I.Set NFAState -> I.Set NFAState
runNFA [] states = states
runNFA (c:cs) states = runNFA cs (step c states)

runNFAFromEverywhere :: [Symbol] -> I.Set NFAState -> I.Set NFAState
runNFAFromEverywhere s start = runNFAFromEverywhere_ s start start

runNFAFromEverywhere_ :: [Symbol] -> I.Set NFAState -> I.Set NFAState -> I.Set NFAState
runNFAFromEverywhere_ [] start states = states `I.union` start
runNFAFromEverywhere_ (c:cs) start states = runNFAFromEverywhere_ cs start (step c states `I.union` start)

accepted :: I.Set NFAState -> Bool
accepted = I.member Accept
