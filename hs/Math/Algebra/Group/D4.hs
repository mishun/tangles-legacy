module Math.Algebra.Group.D4
	(
	  D4(..)
	, Element(..)
	, reflectionRotation
	, rotation
	, hasReflection
	, fromReflectionRotation
	, fromDnElement
	, toDnElement

	, SubGroup(..)
	, hasReflectionElement
	, rotationPeriod
	, toDnSubGroup
	, fromDnSubGroup
	) where

import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe

import qualified Math.Algebra.Group.Dn as Dn

import Math.Algebra.Group


-- Element = (E^mirror) * (C^rotation)                    
--    |3   2|    |0   3|        |3   2|    |1   2|
--    | \ / | C  | \ / |        | \ / | E  | \ / |
-- C: |  *  | -> |  *  |     E: |  *  | -> |  *  |
--    | / \ |    | / \ |        | / \ |    | / \ |
--    |0   1|    |1   2|        |0   1|    |0   3|

data D4 = D4 deriving (Eq)

data Element = I | E | C | EC | C2 | EC2 | C3 | EC3 deriving (Eq, Ord, Show, Read, Bounded, Enum, Ix.Ix)


instance Group D4 Element where
	group _ = D4

	identity _ = I

	inverse a = if hasReflection a
		then a
		else fromReflectionRotation (False, -rotation a)

	(<*>) a b = fromReflectionRotation (m, r)
		where
			m = (hasReflection a) /= (hasReflection b)
			r = if hasReflection b then rb - ra else rb + ra

			ra = rotation a
			rb = rotation b


instance FiniteGroup D4 Element Int where
	groupOrder _ = 8

	groupElements _ = [I .. EC3]


instance Permutation Element Int where
	permutationOrder _ = 4

	permute el ix
		| ix < 0 || ix > 3  = error "permute: out of bound"
		| hasReflection el  = (-ix - rotation el) Bits..&. 3
		| otherwise         = (ix + rotation el) Bits..&. 3


instance PermutationGroup D4 Element Int where
	groupSetOrder _ = 4


reflectionRotation :: Element -> (Bool, Int)
reflectionRotation el =
	case el of
		I   -> (False, 0)
		E   -> (True , 0)
		C   -> (False, 1)
		EC  -> (True , 1)
		C2  -> (False, 2)
		EC2 -> (True , 2)
		C3  -> (False, 3)
		EC3 -> (True , 3)


rotation :: Element -> Int
rotation = snd . reflectionRotation


hasReflection :: Element -> Bool
hasReflection = fst . reflectionRotation


fromReflectionRotation :: (Bool, Int) -> Element
fromReflectionRotation (ref, rot) =
	case rot Bits..&. 3 of
		0 -> if ref then E   else I
		1 -> if ref then EC  else C
		2 -> if ref then EC2 else C2
		3 -> if ref then EC3 else C3
		_ -> I


fromDnElement :: Dn.Element -> Element
fromDnElement el
	| n /= 4     = error "fromDnElement: order is not 4"
	| otherwise  = fromReflectionRotation (Dn.hasReflection el, Dn.rotation el)

	where
		n = permutationOrder el


toDnElement :: Element -> Dn.Element
toDnElement el = Dn.fromReflectionRotation (Dn.D 4) (hasReflection el, rotation el)



data SubGroup = SubD4 | SubC4 | SubGS | SubDS | SubC2 | SubES | SubECS | SubEC2S | SubEC3S | SubID deriving (Eq, Ord, Bounded, Enum, Ix.Ix)


instance FiniteSubGroup SubGroup D4 Element Int where
	containingGroup _ = D4

	cyclicSubGroup el = case el of
		I   -> SubID
		E   -> SubES
		C   -> SubC4
		EC  -> SubECS
		C2  -> SubC2
		EC2 -> SubEC2S
		C3  -> SubC4
		EC3 -> SubEC3S

	factorGroup sg = case sg of
		SubD4   -> Maybe.Just SubID
		SubC4   -> Maybe.Just SubES
		SubGS   -> Maybe.Just SubES
		SubDS   -> Maybe.Just SubC2
		SubC2   -> Maybe.Just SubDS
		SubES   -> Maybe.Nothing
		SubECS  -> Maybe.Nothing
		SubEC2S -> Maybe.Nothing
		SubEC3S -> Maybe.Nothing
		SubID   -> Maybe.Just SubD4
	
	subGroupOrder = length . head . factorSet

	numberOfClasses = length . factorSet

	factorSet sg = case sg of
		SubD4   -> [ [I, E, C, EC, C2, EC2, C3, EC3] ]
		SubC4   -> [ [I,  C, C2,  C3], [E, EC, EC2, EC3] ]
		SubGS   -> [ [I, EC, C2, EC3], [E,  C, EC2,  C3] ]
		SubDS   -> [ [I,  E, C2, EC2], [C, EC,  C3, EC3] ]
		SubC2   -> [ [I,  C2], [E, EC2], [ C,  C3], [EC , EC3] ]
		SubES   -> [ [I,   E], [C, EC3], [EC,  C3], [C2 , EC2] ]
		SubECS  -> [ [I,  EC], [E,   C], [C2, EC3], [EC2,  C3] ]
		SubEC2S -> [ [I, EC2], [E,  C2], [ C,  EC], [C3 , EC3] ]
		SubEC3S -> [ [I, EC3], [E,  C3], [ C, EC2], [EC ,  C2] ]
		SubID   -> [ [I], [E], [C], [EC], [C2], [EC2], [C3], [EC3] ]

	factorSetId subGroup element = factorSetIdLookup Array.! (subGroup, element)

	unionSubGroups a b
		| a == b      = a
		| a > b       = unionSubGroups b a
		| a == SubID  = b
		| b == SubD4  = SubD4
		| otherwise   = List.foldl' extend a $ subGroupElements b

		where
			extend sg el
				| subGroupContainsElement sg el  = sg
				| otherwise  = case sg of
					SubD4   -> SubD4
					SubC4   -> SubD4
					SubGS   -> SubD4
					SubDS   -> SubD4

					SubC2   ->
						if hasReflection el
							then if (1 Bits..&. rotation el) == 1
								then SubGS
								else SubDS
							else SubC4

					SubES   ->
						if el == C2 || el == EC2
							then SubDS
							else SubD4

					SubECS  ->
						if el == C2 || el == EC3
							then SubGS
							else SubD4

					SubEC2S ->
						if el == C2 || el == E
							then SubDS
							else SubD4

					SubEC3S ->
						if el == C2 || el == EC
							then SubGS
							else SubD4

					SubID   ->
						case el of
							C2  -> SubC2
							E   -> SubES
							EC  -> SubECS
							EC2 -> SubEC2S
							EC3 -> SubEC3S
							_   -> SubC4


factorSetIdLookup :: Array.Array (SubGroup, Element) Int
factorSetIdLookup = Array.listArray ((SubD4, I), (SubID, EC3)) $ List.concatMap (ids . factorSet) [SubD4 .. SubID]
	where
		ids classes = snd $ unzip $ List.sort $ List.concatMap zipId $ zip classes [0 ..]

		zipId (cls, ind) = map (\ x -> (x, ind)) cls


instance Show SubGroup where
	show sg = case sg of
		SubD4   -> "D4"
		SubC4   -> "C4"
		SubGS   -> "GS"
		SubDS   -> "DS"
		SubC2   -> "C2"
		SubES   -> "ES"
		SubECS  -> "ECS"
		SubEC2S -> "EC2S"
		SubEC3S -> "EC3S"
		SubID   -> "ID"


hasReflectionElement :: SubGroup -> Bool
hasReflectionElement sg = List.any hasReflection $ subGroupElements sg


rotationPeriod :: SubGroup -> Int
rotationPeriod sg
	| subGroupContainsElement sg C   = 1
	| subGroupContainsElement sg C2  = 2
	| otherwise                      = 4


toDnSubGroup :: SubGroup -> Dn.SubGroup
toDnSubGroup sg = if Maybe.isJust refl
	then Dn.fromPeriodAndMirroredZero 4 period $ permute (Maybe.fromJust refl) 0
	else Dn.fromPeriod 4 period

	where
		refl = List.find hasReflection $ subGroupElements sg
		period = rotationPeriod sg


fromDnSubGroup :: Dn.SubGroup -> SubGroup
fromDnSubGroup sg
	| 4 /= Dn.groupSetOrder' sg  = error "fromDnSubGroup: order is not 4"
	| otherwise  =
		case period of
			1 -> if mirror
				then SubD4
				else SubC4

			2 -> if mirror
				then if mz == 0
					then SubDS
					else SubGS
				else SubC2

			_ -> if mirror
				then case mz of
					0 -> SubES
					1 -> SubEC3S
					2 -> SubEC2S
					_ -> SubECS
				else SubID

	where
		period = Dn.rotationPeriod sg
		mirror = Dn.hasReflectionElement sg
		mz = Dn.mirroredZero sg
