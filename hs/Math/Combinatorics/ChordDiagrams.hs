module Math.Combinatorics.ChordDiagrams
	(
	  ChordDiagram(..)
	, allSites
	, allChords
	, toAdjList
	, toDeltaList
	, toPairingsList
	, lexicographicalCompare
	) where

import qualified Data.Ord as Ord
import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Array as Array


class (Ord cd, Ix.Ix site) => ChordDiagram cd site
		| cd -> site
		, site -> cd
	where
		numberOfChords :: cd -> Int
		numberOfSites  :: cd -> Int
		sites          :: cd -> (site, site)

		position :: site -> Int
		mate     :: site -> site


allSites :: (ChordDiagram cd s) => cd -> [s]
allSites = Ix.range . sites


allChords :: (ChordDiagram cd s) => cd -> [(s, s)]
allChords = filter (\ (a, b) -> a < b) . map (\ s -> (s, mate s)) . allSites


toAdjList :: (ChordDiagram cd s) => cd -> [Int]
toAdjList = map (position . mate) . allSites


toDeltaList :: (ChordDiagram cd s) => cd -> [Int]
toDeltaList cd = map (\ s -> delta s $ mate s) $ allSites cd
	where
		n = numberOfSites cd

		delta a b = mod (n + (position b) - (position a)) n


toPairingsList :: (ChordDiagram cd s) => cd -> [Int]
toPairingsList cd = Array.elems $ Array.array (sites cd) $ List.concatMap (\ ((a, b), i) -> [(a, i), (b, i)]) $ zip (allChords cd) [0 ..]


lexicographicalCompare :: (ChordDiagram cd s) => cd -> cd -> Ordering
lexicographicalCompare = Ord.comparing toDeltaList
