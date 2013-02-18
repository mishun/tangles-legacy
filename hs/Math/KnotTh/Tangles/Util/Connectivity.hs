module Math.KnotTh.Tangles.Util.Connectivity
	(
	  isConnected
	, isPrime
	, pushResidualFlow
	) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.State.Strict as State

import Control.Monad

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Util.Paths


isConnected :: (Tangle t c d ct) => t -> Bool
isConnected tangle = List.all (\ (a, b) -> Set.member a con && Set.member b con) edges
	where
		edges = allEdges tangle

		con = dfs (Set.empty) $ fst $ head edges

		dfs vis c =
			if Set.member c vis
				then vis
				else List.foldl' dfs (Set.insert c vis) neigh

			where
				neigh = if isLeg c
					then [opposite c]
					else [opposite c, nextCCW c, nextCW c]


isPrime :: (Tangle t c d ct) => t -> Bool
isPrime tangle = (connections == List.nub connections)
	where
		idm = Map.fromList $ List.concatMap (\ (face, i) -> zip face $ List.repeat i) $ zip faces [(0 :: Int) ..]
			where
				faces = allFaces tangle

		connections = List.sort $ map getPair $ allEdges tangle
			where
				getPair (da, db) = (min a b, max a b)
					where
						a = idm Map.! da
						b = idm Map.! db


pushResidualFlow :: (Tangle t c d ct) => t -> [c] -> [c] -> Array.Array d Int -> Maybe.Maybe (Array.Array d Int)
pushResidualFlow tangle starts ends flow = (State.evalState bfs initial) >>= push
	where
		initial = (Seq.fromList starts, Map.fromList $ zip starts (List.repeat []))

		endFlag = Array.array (crossingsRange tangle) $! zip (allCrossings tangle) (List.repeat False) ++ zip ends (List.repeat True)

		bfs = do
			empty <- isEmpty
			if empty
				then return $ Maybe.Nothing
				else do
					u <- dequeue
					if endFlag Array.! u
						then do
							p <- getPath u
							return $! Maybe.Just p
						else do
							let ud = filter (\ d -> flow Array.! d < 1) $ incidentDarts u
							let brd = List.find (isLeg . opposite) ud
							if Maybe.isJust brd
								then do
									p <- getPath u
									return $! Maybe.Just $! (Maybe.fromJust brd) : p
								else do
									mapM_ relax ud
									bfs
			where
				relax d = do
					let v = adjacentCrossing d
					vis <- isVisited v
					when (not vis) $ enqueue v d

				isEmpty = State.gets (\ (q, _) -> Seq.null q)

				dequeue = do
					(c Seq.:< rest) <- State.gets (\ (q, _) -> Seq.viewl q)
					State.modify (\ (_, p) -> (rest, p))
					return c

				getPath v = State.gets (\ (_, p) -> p Map.! v)

				enqueue c d = State.modify (\ (q, p) -> (q Seq.|> c, Map.insert c (d : p Map.! (incidentCrossing d)) p))

				isVisited c = State.gets (\ (_, p) -> Map.member c p)

		push path = return $! flow Array.// pathFlow
			where
				pathFlow = List.concatMap dartFlow path

				dartFlow d =
					if isLeg r
						then [df]
						else [df, rf]

					where
						df = (d, (flow Array.! d) + 1)
						r = opposite d
						rf = (r, (flow Array.! r) - 1)
