module Math.Algebra.Group.Dn
	(
	  Dn(..)
	, Element
	, reflectionRotation
	, rotation
	, hasReflection
	, fromRotation
	, fromReflectionRotation
	, fromRotationReflection

	, SubGroup
	, groupSetOrder'
	, containingGroup'
	, rotationPeriod
	, hasReflectionElement
	, isTrivial
	, rotationBasis
	, reflectionBasis
	, mirroredZero
	, adjointDifferenceForBasis
	, fromPeriod
	, fromPeriodAndMirroredZero
	) where

import Math.Algebra.Group


-- Element = (E^mirror) * (C^rotation)

data Dn = D Int deriving (Eq)

data Element = El Int Bool Int deriving (Eq)


instance Group Dn Element where
	group (El n _ _) = D n

	identity (D n) = El n False 0

	inverse (El n False 0) = El n False 0
	inverse (El n False r) = El n False (n - r)
	inverse (El n True r) = El n True r

	(<*>) (El an am ar) (El bn bm br)
		| an /= bn   = error "(*): order conflict"
		| otherwise  = El an resm resr

		where
			resm = am /= bm
			resr = mod (if bm then an + br - ar else ar + br) an


instance FiniteGroup Dn Element Int where
	groupOrder (D n) = n + n

	groupElements (D n) = map (\ (m, r) -> El n m r) [ (m, r) | m <- [False, True], r <- [0 .. (n - 1)] ]


instance Permutation Element Int where
	permutationOrder (El n _ _) = n

	permute (El n m r) ix
		| ix < 0 || ix >= n  = error "permute: out of bound"
		| m                  = mod (n + n - ix - r) n
		| otherwise          = mod (ix + r) n


instance PermutationGroup Dn Element Int where
	groupSetOrder (D n) = n


instance Show Element where
	show (El n m r) = showM ++ "(" ++ (show r) ++ "/" ++ (show n) ++ ")"
		where
			showM = if m then "E" else ""


reflectionRotation :: Element -> (Bool, Int)
reflectionRotation (El _ m r) = (m, r)


rotation :: Element -> Int
rotation (El _ _ r) = r


hasReflection :: Element -> Bool
hasReflection (El _ m _) = m


fromRotation :: Dn -> Int -> Element
fromRotation (D n) r = El n False $ mod (n + mod r n) n


fromReflectionRotation :: Dn -> (Bool, Int) -> Element
fromReflectionRotation (D n) (m, r) = El n m $ mod (n + mod r n) n


fromRotationReflection :: Dn -> (Int, Bool) -> Element
fromRotationReflection (D n) (r, m) = El n m $ mod (n + mod (if m then -r else r) n) n



data SubGroup = Rot { _n, _p :: Int } | Mirr { _n, _p, _md :: Int }


groupSetOrder' :: SubGroup -> Int
groupSetOrder' = _n


containingGroup' :: SubGroup -> Dn
containingGroup' sg = D $ _n sg


rotationPeriod :: SubGroup -> Int
rotationPeriod = _p


hasReflectionElement :: SubGroup -> Bool
hasReflectionElement (Rot _ _) = False
hasReflectionElement _ = True


isTrivial :: SubGroup -> Bool
isTrivial sg = (not $ hasReflectionElement sg) && (rotationPeriod sg == groupSetOrder' sg)


rotationBasis :: SubGroup -> Element
rotationBasis sg = fromRotation (containingGroup' sg) $ rotationPeriod sg


reflectionBasis :: SubGroup -> Element
reflectionBasis sg = fromRotationReflection (containingGroup' sg) (mirroredZero sg, True)


mirroredZero :: SubGroup -> Int
mirroredZero (Rot _ _) = error "no mirror symmetries in this subgroup"
mirroredZero sg = _md sg


adjointDifferenceForBasis :: (Group f e) => (e, e) -> SubGroup -> Element -> Element -> e
adjointDifferenceForBasis (adjRot, adjMir) sg a b
	| (group a /= D n) || (group b /= D n)  = error "adjointDifferenceForBasis: order conflict"
	| mod rotationDiff period /= 0          = error "adjointDifferenceForBasis: elements are not equivalent"
	| otherwise  =
		if needMirror
			then adjRotation <*> adjMir
			else adjRotation

	where
		n = groupSetOrder' sg

		period = rotationPeriod sg

		needMirror = (hasReflection a) /= (hasReflection b)

		toRotate =
			if needMirror
				then (reflectionBasis sg) <*> b
				else b

		rotationDiff = (rotation a) - (rotation toRotate)

		rotationNum =
			let d = div rotationDiff period
			in if hasReflection a then -d else d

		adjRotation = power rotationNum adjRot


fromPeriod :: Int -> Int -> SubGroup
fromPeriod n p
	| n <= 0        = error "order is non-positive"
	| p <= 0        = error "period is non-positive"
	| mod n p /= 0  = error "period does not divide order"
	| otherwise     = Rot n p


fromPeriodAndMirroredZero :: Int -> Int -> Int -> SubGroup
fromPeriodAndMirroredZero n p mz
	| n <= 0        = error "order is non-positive"
	| p <= 0        = error "period is non-positive"
	| mz < 0        = error "mirrored zero is negative"
	| mod n p /= 0  = error "period does not divide order"
	| otherwise     = Mirr n p (mod mz n)
