module Math.Algebra.Group
	(
	  Group(..)
	, FiniteGroup(..)
	, FiniteSubGroup(..)
	, Permutation(..)
	, PermutationGroup(..)

	, isIdentity
	, commutator
	, isCommutate
	, differenceLeft
	, differenceRight
	, conjugate
	, power
	) where

import qualified Data.Maybe as Maybe


class (Eq group, Eq element) => Group group element
		| group -> element
		, element -> group
	where
		group :: element -> group

		identity :: group -> element

		inverse :: element -> element

		(<*>) :: element -> element -> element


class (Group group element, Integral ord) => FiniteGroup group element ord
		| group -> element
		, element -> group
		, group -> ord
	where
		groupOrder :: group -> ord

		groupElements :: group -> [element]


class (Eq subGroup, FiniteGroup group element ord) => FiniteSubGroup subGroup group element ord
		| subGroup -> element
	where
		containingGroup :: subGroup -> group

		cyclicSubGroup :: element -> subGroup

		factorGroup :: subGroup -> Maybe.Maybe subGroup

		subGroupElements :: subGroup -> [element]
		subGroupContainsElement :: subGroup -> element -> Bool

		subGroupOrder, numberOfClasses :: subGroup -> ord

		factorSetRepresentatives :: subGroup -> [element]
		factorSet :: subGroup -> [[element]]
		factorSetId :: subGroup -> element -> ord

		unionSubGroups :: subGroup -> subGroup -> subGroup


		subGroupElements = head . factorSet
		factorSetRepresentatives = (map head) . factorSet
		subGroupContainsElement sg el = (factorSetId sg el) == 0


class (Eq permutation, Integral ord) => Permutation permutation ord
		| permutation -> ord
	where
		permutationOrder :: permutation -> ord
		permute :: permutation -> ord -> ord
		--cycles :: perm -> [[Int]]


class (FiniteGroup group element ord, Permutation element ord) => PermutationGroup group element ord
		| group -> element
		, element -> group
	where
		groupSetOrder :: group -> ord

		groupSetOrder = permutationOrder . identity


isIdentity :: (Group g e) => e -> Bool
isIdentity g = g == (identity $ group g)


commutator :: (Group g e) => e -> e -> e
commutator g h = (inverse g) <*> (inverse h) <*> g <*> h


isCommutate :: (Group g e) => e -> e -> Bool
isCommutate g h = (commutator g h) == (identity $ group g)


differenceLeft :: (Group g e) => e -> e -> e
differenceLeft a b = a <*> (inverse b)


differenceRight :: (Group g e) => e -> e -> e
differenceRight a b = (inverse b) <*> a


conjugate :: (Group g e) => e -> e -> e
conjugate h g = (inverse h) <*> g <*> h


power :: (Integral ord, Group g e) => ord -> e -> e
power p x
	| p == 0    = e
	| p < 0     = power (-p) $ inverse x
	| otherwise = mul (p, e, x)

	where
		e = identity $ group x

		mul (0, acc, _) = acc
		mul (n, acc, sq) = mul (half, newAcc, sq <*> sq)
			where
				(half, rest) = divMod n 2

				newAcc
					| rest == 1  = sq <*> acc
					| otherwise  = acc
