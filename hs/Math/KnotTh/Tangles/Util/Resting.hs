module Math.KnotTh.Tangles.Util.Resting
	(
	  restingPart
	) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Control.Monad.State.Strict as State

import Control.Monad

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Util.Connectivity


zeroFlow :: (Tangle t c d ct) => t -> Array.Array d Int
zeroFlow tangle = Array.listArray (dartsRange tangle) $ List.repeat (0 :: Int)


restingPart :: (Tangle t c d ct) => t -> [d] -> Maybe.Maybe ([d], Array.Array c Bool)
restingPart tangle incoming
	| null incoming            = error "restingPart: no darts"
	| List.any isLeg incoming  = error "restingPart: leg passed"
	| not startsDifferent      = Maybe.Nothing
	| otherwise                = maxFlow >>= getSubtangle >>= checkConnectivity >>= outcoming

	where
		m = length incoming

		starts = map incidentCrossing incoming

		ends = Maybe.mapMaybe maybeAdjacentCrossing incoming

		startsDifferent = List.all (\ (a, b) -> a /= b) $ zip starts (tail starts)

		maxFlow = push (0 :: Int) flow0
			where
				flow0 = let blockingFlow = List.concatMap (\ l -> if isLeg l then [] else [(l, 1)]) (incoming ++ map opposite incoming)
					in (zeroFlow tangle) Array.// blockingFlow

				push flowValue flow
					| flowValue > m             = Maybe.Nothing
					| Maybe.isNothing nextFlow  = Maybe.Just (flow, flowValue)
					| otherwise                 = push (flowValue + 1) (Maybe.fromJust nextFlow)

					where
						nextFlow = pushResidualFlow tangle starts ends flow

		getSubtangle (flow, flowValue) = Maybe.Just $ (result, flowValue)
			where
				result = Array.listArray (crossingsRange tangle) $ map (\ c -> Set.member c subtangle) $ allCrossings tangle

				subtangle = State.execState (forM_ starts dfs) Set.empty

				dfs c = do
					visited <- State.gets $ Set.member c
					when (not visited) $ do
						State.modify $ Set.insert c
						mapM_ (dfs . adjacentCrossing) $ filter (\ d -> (flow Array.! d) < 1) $ incidentDarts c

		checkConnectivity (sub, flowValue) =
			if List.all (\ s -> Set.member s mask) $ tail starts
				then Maybe.Just (sub, flowValue)
				else Maybe.Nothing

			where
				mask = State.execState (dfs $ head starts) Set.empty

				dfs c = do
					visited <- State.gets $ Set.member c
					when (not visited) $ do
						State.modify $ Set.insert c
						mapM_ dfs $ filter ((Array.!) sub) $ Maybe.mapMaybe maybeAdjacentCrossing $ incidentDarts c

		outcoming (sub, flowValue)
			| List.any (not . onBorder) incoming  = Maybe.Nothing
			| flowValue /= length result          = Maybe.Nothing
			| otherwise                           = Maybe.Just $! (result, sub)

			where
				result = restoreOutcoming (opposite lastIncoming) []

				firstIncoming = head incoming
				lastIncoming = last incoming

				onBorder xy = (not $ isLeg xy) && (sub Array.! x) && (isLeg yx || (not $ (Array.!) sub y))
					where
						yx = opposite xy

						x = incidentCrossing xy
						y = incidentCrossing yx

				traverseNext = nextCW . opposite

				restoreOutcoming d out
					| d == firstIncoming  = out
					| onBorder d          = restoreOutcoming (opposite d) (d : out)
					| otherwise           = restoreOutcoming (traverseNext d) out
