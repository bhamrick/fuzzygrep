module Fuzzy where

import NFA hiding (start, end)
import CompactNFA

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

fuzz :: Int -> CompactNFA -> CompactNFA
fuzz k cnfa = CompactNFA
  { start = IS.map (* (k+1)) (start cnfa)
  , transition = let tr = transition cnfa in IM.insert accept (const $ IS.singleton accept) $
        IM.unions (map (\x -> makeLayer (k+1) x (fuzz_next cnfa) tr) [0..k])
  , fuzz_next = fuzzAllNext (k+1) (fuzz_next cnfa)
  }

fuzzAllNext :: Int -> IM.IntMap IS.IntSet -> IM.IntMap IS.IntSet
fuzzAllNext factor = IM.foldWithKey (fuzzSingleAllNext factor (factor - 1)) (IM.singleton accept (IS.singleton accept))

fuzzSingleAllNext :: Int -> Int -> IM.Key -> IS.IntSet -> IM.IntMap IS.IntSet -> IM.IntMap IS.IntSet
fuzzSingleAllNext _ (-1) _ _ = id
fuzzSingleAllNext _ _ (-1) _ = id
fuzzSingleAllNext factor offset key next = if offset == factor - 1
    then IM.insert (factor * key + offset) (IS.map (\x -> factor * x + offset) next) . fuzzSingleAllNext factor (offset - 1) key next
    else IM.insert (factor * key + offset) (IS.map (\x -> factor * x + offset) next `IS.union` IS.map (\x -> factor * x + offset + 1) next) . fuzzSingleAllNext factor (offset - 1) key next

makeLayer :: Int -> Int -> IM.IntMap IS.IntSet -> IM.IntMap (Symbol -> IS.IntSet) -> IM.IntMap (Symbol -> IS.IntSet)
makeLayer factor offset an = IM.foldWithKey (fuzzTransition factor offset an) IM.empty

fuzzTransition :: Int -> Int -> IM.IntMap IS.IntSet -> IM.Key -> (Symbol -> IS.IntSet) -> IM.IntMap (Symbol -> IS.IntSet) -> IM.IntMap (Symbol -> IS.IntSet)
fuzzTransition factor offset an key f = if offset == factor - 1
    then IM.insert (factor * key + offset) (IS.map (\x -> factor * x + offset) . f)
    else IM.insert (factor * key + offset) (\c -> if isLetter c
        then IS.union (IS.map (\x -> factor * x + offset + 1) (an IM.! key)) . (IS.map (\x -> factor * x + offset) . f) $ c
        else IS.map (\x -> factor * x + offset) . f $ c)
