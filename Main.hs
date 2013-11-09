module Main where

import Control.Monad
import System.IO

import Regex
import CompactNFA
import Fuzzy

testcase n = (join $ replicate n "a?") ++ replicate n 'a'

main :: IO ()
main = do
    n <- readLn
    let regex = compileRegex $ testcase n
    print $ matchesRegex regex $ replicate n 'a'
    print $ matchesRegex regex $ replicate (n-1) 'a'
