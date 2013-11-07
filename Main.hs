module Main where

import System.IO

import qualified Data.Set as S

import NFA

testcase :: Int -> [NFAState]
testcase n = do
    let phase1 = map (\i -> zeroOrOne $ return . Letter i (Chr 'a')) [1 .. n]
    let phase2 = map (\i -> return . Letter i (Chr 'a')) [n+1 .. 2*n]
    foldr (.) (id) (phase1 ++ phase2) $ [Accept]

main :: IO ()
main = do
    let n = 100
    let nfa = S.fromList $ testcase n
    -- putStrLn . show $ nfa
    let string1 = map Chr $ replicate n 'a'
    let string2 = map Chr $ replicate (n-1) 'a'
    putStrLn . show . accepted $ runNFA string1 nfa
    putStrLn . show . accepted $ runNFA string2 nfa
