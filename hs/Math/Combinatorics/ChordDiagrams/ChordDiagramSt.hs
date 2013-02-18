module Math.Combinatorics.ChordDiagrams.ChordDiagramSt
	(
	  ChordDiagramSt
	, Site
	, fromAdjList
	, fromDeltaList
	, fromPairingsList
	, clone
	) where

import qualified Data.Ord as Ord
import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Array as Array

import qualified Util.IxWith as IxWith

import Math.Combinatorics.ChordDiagrams


data ChordDiagramSt = CD Int (Array.Array Int Int) deriving (Eq)

data Site = Site
	{
		_owner :: ChordDiagramSt,
		_index :: Int
	}


instance Ord ChordDiagramSt where
	compare = lexicographicalCompare


instance Eq Site where
	(==) a b = (_index a) == (_index b)


instance Ord Site where
	compare = Ord.comparing _index


instance Ix.Ix Site where
	range (a, b) = IxWith.rangeWith (_index, Site (_owner a)) (a, b)

	index = IxWith.indexWith _index

	inRange = IxWith.inRangeWith _index

	rangeSize = IxWith.rangeSizeWith _index


instance ChordDiagram ChordDiagramSt Site where
	numberOfChords (CD n _) = n

	numberOfSites (CD n _) = 2 * n

	sites cd@(CD n _) = (Site cd 0, Site cd $ 2 * n - 1)

	position = _index

	mate s = Site cd $ a Array.! i
		where
			cd@(CD _ a) = _owner s
			i = _index s


fromAdjList :: [Int] -> ChordDiagramSt
fromAdjList list
	| odd l      = error "fromAdjList: list must have even length"
	| not ok     = error "fromAdjList: bad structure"
	| otherwise  = CD n cd

	where
		l = length list
		n = div l 2

		cd = Array.listArray (0, l - 1) list

		ok = List.all (\ (i, c) -> i == (cd Array.! c)) $ Array.assocs cd


fromDeltaList :: [Int] -> ChordDiagramSt
fromDeltaList list = fromAdjList $ map (\ (d, i) -> mod (i + d) l) $ zip list [0 ..]
	where
		l = length list


fromPairingsList :: (Ord a) => [a] -> ChordDiagramSt
fromPairingsList list
	| odd l      = error "fromPairingsList: list must have even length"
	| otherwise  = CD n $ Array.array (0, l - 1) cd

	where
		l = length list
		n = div l 2

		groups = List.groupBy (\ (a, _) (b, _) -> a == b) $ List.sort $ zip list [0 ..]

		cd = List.concatMap makeChord groups

		makeChord gr
			| length gr /= 2  = error "fromPairingsList: bad list structure"
			| otherwise       = [(i, j), (j, i)]

			where
				i = snd $ gr !! 0
				j = snd $ gr !! 1


clone :: (ChordDiagram cd s) => cd -> ChordDiagramSt
clone = fromAdjList . toAdjList
