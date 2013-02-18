module Math.Algebra.Group.Sn
	(
	  Sn(..)
	, Perm
	, fromList
	) where

import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Array as Array

import Math.Algebra.Group


data (Integral ord, Ix.Ix ord) => Sn ord = S ord deriving (Eq)

data (Integral ord, Ix.Ix ord) => Perm ord = Perm (Array.Array ord ord) deriving (Eq)


instance (Integral ord, Ix.Ix ord) => Group (Sn ord) (Perm ord) where
	group p = S $ permutationOrder p

	identity (S n) = Perm $ Array.listArray (0, n - 1) [0 .. (n - 1)]

	inverse a@(Perm p) =
		let n = permutationOrder a
		in Perm $ Array.array (0, n - 1) $ map (\ (x, y) -> (y, x)) $ Array.assocs p

	(<*>) a@(Perm pa) b@(Perm pb)
		| n /= m     = error $ "(*): order conflict " ++ (show n) ++ " - " ++ (show m)
		| otherwise  = Perm $ Array.listArray (0, n - 1) list

		where
			n = permutationOrder a
			m = permutationOrder b

			list = map (((Array.!) pa) . ((Array.!) pb)) [0 .. (n - 1)]


instance (Integral ord, Ix.Ix ord) => FiniteGroup (Sn ord) (Perm ord) ord where
	groupOrder (S n) = product [1 .. n]

	groupElements (S n) = map make $ List.permutations [0 .. (n - 1)]

 
instance (Integral ord, Ix.Ix ord) => Permutation (Perm ord) ord where
	permutationOrder (Perm p) = 1 + (snd $ Array.bounds p)

	permute (Perm p) i = p Array.! i


instance (Integral ord, Ix.Ix ord) => PermutationGroup (Sn ord) (Perm ord) ord where
	groupSetOrder (S n) = n


make :: (Integral ord, Ix.Ix ord) => [ord] -> Perm ord
make list = Perm $ Array.listArray (0, n - 1) list
	where
		n = List.genericLength list


fromList :: (Integral ord, Ix.Ix ord) => [ord] -> Perm ord
fromList list =
	if ok
		then Perm $ Array.listArray (0, n - 1) list
		else error "fromList: not a permutation"

	where
		n = List.genericLength list
		ok = (List.sort list) == [0 .. n - 1]
