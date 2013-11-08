module Regex where

import NFA
import qualified Intable as I

-- I'm going to ignore the | operator for now because it is annoying

-- First we'll tokenize the string
-- This involves dealing with escapes, letter groups, and so on
data Token = Letter Char
           | LetterSet [Char]
           | LetterAntiSet [Char]
           | Wildcard
           | ZeroOrOne
           | ZeroOrMore
           | OneOrMore
           | LeftParen
           | RightParen
           | StartToken
           | EndToken
           deriving (Eq, Show, Ord)

tokenize :: String -> [Token]
tokenize "" = []
tokenize s = let (t, rest) = getToken s in t : tokenize rest

buildNFA :: [Token] -> Int -> [I.Set NFAState -> I.Set NFAState] -> (I.Set NFAState -> I.Set NFAState) -> I.Set NFAState -> I.Set NFAState
buildNFA [] _ bs lst = (foldr (.) id bs) . lst
buildNFA (t:ts) i (b:bs) lst = case t of
    Letter c -> buildNFA ts (i+1) ((b . lst):bs) (letter i (Chr c))
    LetterSet s -> buildNFA ts (i+1) ((b . lst):bs) (letters i (map Chr s))
    LetterAntiSet s -> buildNFA ts (i+1) ((b . lst):bs) (notLetters i (map Chr s))
    Wildcard -> buildNFA ts (i+1) ((b . lst):bs) (wildcard i)
    ZeroOrOne -> buildNFA ts (i+1) (b:bs) (zeroOrOne lst)
    ZeroOrMore -> buildNFA ts (i+1) (b:bs) (zeroOrMore lst)
    OneOrMore -> buildNFA ts (i+1) (b:bs) (oneOrMore lst)
    LeftParen -> buildNFA ts (i+1) (id:(b . lst):bs) id
    RightParen -> buildNFA ts (i+1) bs (b . lst)
    StartToken -> buildNFA ts (i+1) ((b . lst):bs) (letter i Start)
    EndToken -> buildNFA ts (i+1) ((b . lst):bs) (letter i End)

compileRegex :: String -> I.Set NFAState
compileRegex regex = buildNFA (tokenize regex) 0 [id] id (I.singleton Accept)

matchesRegex :: I.Set NFAState -> String -> Bool
matchesRegex start s = do
    let str = [Start] ++ (map Chr s) ++ [End]
    let final = runNFAFromEverywhere str start
    Accept `I.member` final

whitespaceSet = " \t\n\t\r"
wordSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
digitSet = "0123456789"

getToken :: String -> (Token, String)
getToken "" = error "No next token!"
getToken (c:cs) = case c of
    '\\' -> case head cs of
        'n' -> (Letter '\n', tail cs)
        't' -> (Letter '\t', tail cs)
        'r' -> (Letter '\r', tail cs)
        '\\' -> (Letter '\\', tail cs)
        '[' -> (Letter '[', tail cs)
        ']' -> (Letter ']', tail cs)
        '(' -> (Letter '(', tail cs)
        ')' -> (Letter ')', tail cs)
        '?' -> (Letter '?', tail cs)
        '*' -> (Letter '*', tail cs)
        '+' -> (Letter '+', tail cs)
        '.' -> (Letter '.', tail cs)
        '^' -> (Letter '^', tail cs)
        '$' -> (Letter '$', tail cs)
        's' -> (LetterSet whitespaceSet, tail cs)
        'w' -> (LetterSet wordSet, tail cs)
        'd' -> (LetterSet digitSet, tail cs)
        'S' -> (LetterAntiSet whitespaceSet, tail cs)
        'W' -> (LetterAntiSet wordSet, tail cs)
        'D' -> (LetterAntiSet digitSet, tail cs)
        _ -> error "Bad pattern"
    '[' -> case head cs of
        '^' -> let (s, rest) = getLetterSet (tail cs) in (LetterAntiSet s, rest)
        _ -> let (s, rest) = getLetterSet cs in (LetterSet s, rest)
    '(' -> (LeftParen, cs)
    ')' -> (RightParen, cs)
    '?' -> (ZeroOrOne, cs)
    '*' -> (ZeroOrMore, cs)
    '+' -> (OneOrMore, cs)
    '.' -> (Wildcard, cs)
    '^' -> (StartToken, cs)
    '$' -> (EndToken, cs)
    _ -> (Letter c, cs)

getLetterSet :: String -> ([Char], String)
getLetterSet (c:cs) = case c of
    ']' -> ([], cs)
    '\\' -> let (s, rest) = getLetterSet (tail cs) in case head cs of
        'n' -> ('\n' : s, rest)
        'r' -> ('\r' : s, rest)
        't' -> ('\t' : s, rest)
        '\\' -> ('\\' : s, rest)
        '[' -> ('[' : s, rest)
        ']' -> (']' : s, rest)
        _ -> error "Bad pattern"
    _ -> let (s, rest) = getLetterSet cs in (c : s, rest)
    -- Todo: Things like 0-9
