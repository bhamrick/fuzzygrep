module Main where

import Control.Monad

import System.Environment
import System.IO

import Regex
import CompactNFA
import Fuzzy

data Source = Stdin | File String deriving (Eq, Show, Ord)

data Arguments = Arguments
  { pattern :: String
  , source :: Source
  } deriving (Eq, Show, Ord)

data Options = Options { fuzziness :: Int } deriving (Eq, Show, Ord)

defaultOptions = Options { fuzziness = 0 }

main :: IO ()
main = do
    args_ <- getArgs
    let (opts, rest) = parseOpts args_
    case parseArgs rest of
        Nothing -> usage
        Just args -> do
            let regex = fuzz (fuzziness opts) $ compileRegex (pattern args)
            hdl <- openSource (source args)
            go regex hdl 

openSource :: Source -> IO Handle
openSource Stdin = return stdin
openSource (File fname) = openFile fname ReadMode

go :: CompactNFA -> Handle -> IO ()
go pattern hdl = do
    eof <- hIsEOF hdl
    if eof then return ()
           else do
                line <- hGetLine hdl
                runRegex pattern line
                go pattern hdl

runRegex :: CompactNFA -> String -> IO ()
runRegex regex str = if matchesRegex regex str then putStrLn str else return ()

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " pattern [filename]"

parseOpts :: [String] -> (Options, [String])
parseOpts [] = (defaultOptions, [])
parseOpts (fst:[]) = (defaultOptions, fst:[])
parseOpts (fst:snd:rest) = case fst of
    "-f" -> let (opts, args) = parseOpts rest in (opts { fuzziness = read snd }, args)
    _ -> let (opts, args) = parseOpts (snd:rest) in (opts, fst:args)

parseArgs :: [String] -> Maybe Arguments
parseArgs [] = Nothing
parseArgs (pattern:[]) = Just $ Arguments { pattern = pattern, source = Stdin }
parseArgs (pattern:fname:_) = Just $ Arguments { pattern = pattern, source = File fname }
