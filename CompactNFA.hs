module CompactNFA where

import NFA hiding (start, end)
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Intable as I

-- Here we compactify an NFA so instead of dealing with state objects,
-- we only need to deal with their integral IDs. 

accept = -1

data CompactNFA = CompactNFA
  { start :: IS.IntSet
  , transition :: IM.IntMap (Symbol -> IS.IntSet)
  , fuzz_next :: IM.IntMap IS.IntSet
  }

compactify :: I.Set NFAState -> CompactNFA
compactify startStates = CompactNFA
  { start = idSet startStates
  , transition = buildTransition startStates
  , fuzz_next = buildNext startStates
  }

idSet :: I.Set NFAState -> IS.IntSet
idSet = IS.fromList . map state_id . I.elems

buildTransition :: I.Set NFAState -> IM.IntMap (Symbol -> IS.IntSet)
buildTransition start = I.fold explore (IM.singleton accept (const $ IS.singleton accept)) start

explore :: NFAState -> IM.IntMap (Symbol -> IS.IntSet) -> IM.IntMap (Symbol -> IS.IntSet)
explore s m = case state_id s `IM.member` m of
    True -> m
    False -> case s of
        Accept -> IM.insert accept (const $ IS.singleton accept) m
        Transition i n _ f -> I.fold explore (IM.insert i (idSet . f) m) n

buildNext :: I.Set NFAState -> IM.IntMap IS.IntSet
buildNext start = I.fold explore2 (IM.singleton accept (IS.singleton accept)) start

explore2 :: NFAState -> IM.IntMap IS.IntSet -> IM.IntMap IS.IntSet
explore2 s m = case state_id s `IM.member` m of
    True -> m
    False -> case s of
        Accept -> IM.insert accept (IS.singleton accept) m
        Transition i n b f -> I.fold explore2 (if b then IM.insert i (idSet n) m else IM.insert i (IS.empty) m) n

singleStep :: IM.IntMap (Symbol -> IS.IntSet) -> Symbol -> Int -> IS.IntSet
singleStep tr c s = (tr IM.! s) c

step :: IM.IntMap (Symbol -> IS.IntSet) -> Symbol -> IS.IntSet -> IS.IntSet
step tr c states = IS.unions $ map (CompactNFA.singleStep tr c) (IS.elems states)

run :: [Symbol] -> CompactNFA -> Bool
run s c = accept `IS.member` run_ s (transition c) (start c)

run_ :: [Symbol] -> IM.IntMap (Symbol -> IS.IntSet) -> IS.IntSet -> IS.IntSet
run_ [] _ states = states
run_ (c:cs) tr states = run_ cs tr (CompactNFA.step tr c states)

runFromEverywhere :: [Symbol] -> CompactNFA -> Bool
runFromEverywhere s c = accept `IS.member` runFromEverywhere_ s (transition c) (start c) (start c)

runFromEverywhere_ :: [Symbol] -> IM.IntMap (Symbol -> IS.IntSet) -> IS.IntSet -> IS.IntSet -> IS.IntSet
runFromEverywhere_ [] _ _ states = states
runFromEverywhere_ (c:cs) tr start states = runFromEverywhere_ cs tr start (CompactNFA.step tr c states `IS.union` start)
