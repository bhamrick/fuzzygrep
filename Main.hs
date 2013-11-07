module Main where

import System.IO

import qualified Intable as I

import NFA

testcase :: Int -> I.Set NFAState
testcase n = do
    let phase1 = map (\i -> zeroOrOne $ I.singleton . Letter i (Chr 'a')) [1 .. n]
    let phase2 = map (\i -> I.singleton . Letter i (Chr 'a')) [n+1 .. 2*n]
    foldr (.) (id) (phase1 ++ phase2) $ I.singleton Accept

main :: IO ()
main = do
    n <- readLn
    let nfa = testcase n
    -- putStrLn . show $ nfa
    let string1 = map Chr $ replicate n 'a'
    let string2 = map Chr $ replicate (n-1) 'a'
    putStrLn . show . accepted $ runNFA string1 nfa
    putStrLn . show . accepted $ runNFA string2 nfa
