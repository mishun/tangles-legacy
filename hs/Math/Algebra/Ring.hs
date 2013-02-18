module Math.Algebra.Ring
	(
	  Ring(..)
	, Z(..)
	, commutator
	, isCommutate
	, power
	) where


class (Eq ring, Eq element) => Ring ring element
		| ring -> element
		, element -> ring
	where
		ring :: element -> ring

		zero, unity :: ring -> element

		negative :: element -> element

		(<+>), (<->), (<*>) :: element -> element -> element


data Z = Z deriving (Eq)


instance Ring Z Integer where
	ring _ = Z
	zero _ = 0
	unity _ = 1
	negative a = -a
	(<+>) = (+)
	(<->) = (-)
	(<*>) = (*)


commutator :: (Ring r e) => e -> e -> e
commutator a b = (a <*> b) <-> (b <*> a)


isCommutate :: (Ring r e) => e -> e -> Bool
isCommutate a b = (commutator a b) == (zero $ ring a)


power :: (Integral ord, Ring r e) => ord -> e -> e
power p x
	| p == 0    = e
	| p < 0     = error "power for ring must be non-negative"
	| otherwise = mul p e x

	where
		e = unity $ ring x

		mul 0 acc _ = acc
		mul n acc sq = mul half newAcc (sq <*> sq)
			where
				(half, rest) = divMod n 2

				newAcc
					| rest == 1  = sq <*> acc
					| otherwise  = acc
