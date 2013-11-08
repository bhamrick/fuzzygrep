module Intable where

import qualified Data.IntMap as M

-- An `Intable` is a type whose values can be defined by an Int
class Intable a where
    toInt :: a -> Int

instance Intable Int where
    toInt = id

data Set a = IndirectSet (M.IntMap a) deriving (Eq, Show)

underlying_map (IndirectSet m) = m

(\\) (IndirectSet m1) (IndirectSet m2) = IndirectSet (m1 M.\\ m2)

null (IndirectSet m) = M.null m
size (IndirectSet m) = M.size m
member x (IndirectSet m) = M.member (toInt x) m
notMember x (IndirectSet m) = M.notMember (toInt x) m
isSubsetOf (IndirectSet m1) (IndirectSet m2) = M.isSubmapOf m1 m2
isProperSubsetOf (IndirectSet m1) (IndirectSet m2) = M.isProperSubmapOf m1 m2

empty = IndirectSet M.empty
singleton x = IndirectSet (M.singleton (toInt x) x)
insert x (IndirectSet m) = IndirectSet (M.insert (toInt x) x m)
delete x (IndirectSet m) = IndirectSet (M.delete (toInt x) m)

union (IndirectSet m1) (IndirectSet m2) = IndirectSet (M.union m1 m2)
unions sets = IndirectSet (M.unions (Prelude.map underlying_map sets))
difference (IndirectSet m1) (IndirectSet m2) = IndirectSet (M.difference m1 m2)
intersection (IndirectSet m1) (IndirectSet m2) = IndirectSet (M.intersection m1 m2)

filter f (IndirectSet m) = IndirectSet (M.filter f m)
partition f (IndirectSet m) = let (m1, m2) = M.partition f m in (IndirectSet m1, IndirectSet m2)
-- split, splitMember?

findMin (IndirectSet m) = M.findMin m
findMax (IndirectSet m) = M.findMax m
deleteMin (IndirectSet m) = IndirectSet (M.deleteMin m)
deleteMax (IndirectSet m) = IndirectSet (M.deleteMax m)
-- deleteFindMin, deleteFindMax?
-- minView, maxView?

map f (IndirectSet m) = IndirectSet (M.map f m)
fold f x (IndirectSet m) = M.fold f x m
elems (IndirectSet m) = M.elems m
toList = elems
fromList xs = M.fromList $ Prelude.map (\x -> (toInt x, x)) xs

setBind :: (Intable a, Intable b) => Set a -> (a -> Set b) -> Set b
setBind xs f = unions (Prelude.map f (elems xs))
-- More?

