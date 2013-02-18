module Math.KnotTh.Tangles.Util.Paths
	(
	  containingDirectedPath
	, containingUndirectedPath
	, directedPathsDecomposition
	, undirectedPathsDecomposition
	, containingThread
	, allThreads
	, containingFaceLeft
	, containingFaceRight
	, allFaces
	) where

import qualified Data.List as List
import qualified Data.Set as Set

import Math.KnotTh
import Math.KnotTh.Tangles


containingDirectedPath :: (Tangle t c d ct) => (d -> d, d -> d) -> d -> [d]
containingDirectedPath (adjForward, adjBackward) start =
	if isCycle
		then forward
		else walkBackward (start, forward)

	where
		(forward, isCycle) = walkForward start

		walkForward d
			| isLeg opp     = ([d], False)
			| start == nxt  = ([d], True)
			| otherwise     = (d : nextPath, nextCycle)

			where
				opp = opposite d
				nxt = adjForward opp
				(nextPath, nextCycle) = walkForward nxt

		walkBackward (d, path) =
			if isLeg d
				then path
				else walkBackward (prev, prev : path)

			where
				prev = opposite $ adjBackward d


containingUndirectedPath :: (Tangle t c d ct) => (d -> d) -> d -> [(d, d)]
containingUndirectedPath cont = map (\ d -> (d, opposite d)) . containingDirectedPath (cont, cont)


directedPathsDecomposition :: (Tangle t c d ct) => (d -> d, d -> d) -> t -> [[d]]
directedPathsDecomposition continue = fst . List.foldl' processDart ([], Set.empty) . allLegsAndDarts
	where
		processDart (paths, s) d =
			if Set.member d s
				then (paths, s)
				else (path : paths, nextS)

			where
				path = containingDirectedPath continue d
				nextS = List.foldl' (\ curs a -> Set.insert a curs) s path


undirectedPathsDecomposition :: (Tangle t c d ct) => (d -> d) -> t -> [[(d, d)]]
undirectedPathsDecomposition continue = fst . List.foldl' processDart ([], Set.empty) . allLegsAndDarts
	where
		processDart (paths, s) d =
			if Set.member d s
				then (paths, s)
				else (path : paths, nextS)

			where
				path = containingUndirectedPath continue d
				nextS = List.foldl' (\ curs (a, b) -> Set.insert b $ Set.insert a curs) s path


containingThread :: (Tangle t c d ct) => d -> [(d, d)]
containingThread = containingUndirectedPath continuation


allThreads :: (Tangle t c d ct) => t -> [[(d, d)]]
allThreads = undirectedPathsDecomposition continuation


containingFaceLeft :: (Tangle t c d ct) => d -> [d]
containingFaceLeft d = containingDirectedPath (nextCW, nextCCW) d


containingFaceRight :: (Tangle t c d ct) => d -> [d]
containingFaceRight d = containingDirectedPath (nextCCW, nextCW) d


allFaces :: (Tangle t c d ct) => t -> [[d]]
allFaces = directedPathsDecomposition (nextCW, nextCCW)
