module Data.EquivalenceClasses
	(
	  EquivalenceClasses
	, null
	, size
	, member
	, notMember
	, empty
	, singleton
	, insert
	, join
	, join'
	, find
	, sameClass
	, keys
	, keysSet
	, classes
	, assocs
	) where

import Prelude hiding (null)

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.DisjointSet


data EquivalenceClasses k v =
	EC
	{
		_merger :: !(v -> v -> v),
		_free :: !Int,
		_indices :: !(Map.Map k Int),
		_info :: !(Map.Map Int v),
		_sets :: !DisjointSet
	}


null :: EquivalenceClasses k v -> Bool
null = Map.null . _indices


size :: EquivalenceClasses k v -> Int
size = Map.size . _indices


member :: (Ord k) => k -> EquivalenceClasses k v -> Bool
member key = Map.member key . _indices


notMember :: (Ord k) => k -> EquivalenceClasses k v -> Bool
notMember key = Map.notMember key . _indices


empty :: (v -> v -> v) -> EquivalenceClasses k v
empty merger = EC merger 0 Map.empty Map.empty $ fromBound 7


singleton :: (Ord k) => (v -> v -> v) -> k -> v -> EquivalenceClasses k v
singleton merger key value = insert key value (empty merger)


insert :: (Ord k) => k -> v -> EquivalenceClasses k v -> EquivalenceClasses k v
insert key value (EC merger free indices info disj)
	| exists     = EC merger free indices nextInfo disj
	| otherwise  =
		let nextDisj =
			if free <= bound disj
				then disj
				else reBound (2 * (bound disj) + 1) disj

		in EC merger (free + 1) (Map.insert key free indices) nextInfo nextDisj

	where
		exists = Map.member key indices

		index = if exists
			then findSet disj $! (indices Map.! key)
			else free

		nextInfo = Map.alter alter index info
			where
				alter (Just !x) = Just $! merger x value
				alter Nothing = Just value


join :: (Ord k) => (k, k) -> EquivalenceClasses k v -> EquivalenceClasses k v
join (a, b) m@(EC merger free indices info disjSet)
	| Maybe.isNothing indexA  = m
	| Maybe.isNothing indexB  = m
	| rootA == rootB          = m
	| otherwise  = EC merger free indices newInfo newSet

	where
		indexA = Map.lookup a indices
		indexB = Map.lookup b indices

		rootA = findSet disjSet $ Maybe.fromJust indexA
		rootB = findSet disjSet $ Maybe.fromJust indexB

		newSet = unionSet (rootA, rootB) disjSet

		value = merger (info Map.! rootA) (info Map.! rootB)

		newInfo =
			if rootA == findSet newSet rootA
				then Map.insert rootA value $ Map.delete rootB info
				else Map.insert rootB value $ Map.delete rootA info


join' :: (Ord k) => EquivalenceClasses k v -> (k, k) -> EquivalenceClasses k v
join' m p = join p m


find :: (Ord k) => k -> EquivalenceClasses k v -> v
find key (EC _ _ indices info disjSet) = info Map.! root
	where
		index = (Map.!) indices key

		root = findSet disjSet index


sameClass :: (Ord k) => EquivalenceClasses k v -> k -> k -> Bool
sameClass m a b
	| Maybe.isNothing indexA  = False
	| Maybe.isNothing indexB  = False
	| otherwise  = rootA == rootB

	where
		indices = _indices m
		disjSet = _sets m

		indexA = Map.lookup a indices
		indexB = Map.lookup b indices

		rootA = findSet disjSet $ Maybe.fromJust indexA
		rootB = findSet disjSet $ Maybe.fromJust indexB


keys :: EquivalenceClasses k v -> [k]
keys = Map.keys . _indices


keysSet :: (Ord k) => EquivalenceClasses k v -> Set.Set k
keysSet = Map.keysSet . _indices


classes :: EquivalenceClasses k v -> [v]
classes m = Map.elems $ _info m


assocs :: (Ord k) => EquivalenceClasses k v -> [(k, v)]
assocs ec = map (\ k -> (k, (_info ec) Map.! (findSet (_sets ec) $ (_indices ec) Map.! k))) $ keys ec
