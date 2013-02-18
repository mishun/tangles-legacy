module Math.KnotTh.Tangles.Moves.Pass
	(
	  neighbours
	) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Control.Monad

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles.Util.Resting
import Math.KnotTh.Tangles.Moves.Moves
import Math.KnotTh.Tangles.Moves.ReidemeisterReduction


neighbours :: (Show t, Tangle t c d ArbitraryCrossing) => t -> [(TangleSt.TangleSt ArbitraryCrossing, Int)]
neighbours tangle = Maybe.mapMaybe tryPass $ allDarts tangle
	where
		tryPass ab
			| isLeg ca   = Maybe.Nothing
			| otherwise  = searchPass ba [ca] (passOver ab)

			where
				ac = nextCW ab
				ca = opposite ac
				ba = opposite ab

		searchPass ba incoming passType
			| isLeg ba || a == b       = Maybe.Nothing
			| passOver ba /= passType  = Maybe.Nothing
			| isLeg db || c == d       = Maybe.Nothing
			| db == head incoming      = Maybe.Nothing
			| Maybe.isJust pass        = pass
			| otherwise                = searchPass (opposite $ continuation ba) nextIncoming passType

			where
				ca = last incoming
				ac = opposite ca

				a = incidentCrossing ac 
				b = incidentCrossing ba

				bd = nextCCW ba
				db = opposite bd

				c = incidentCrossing ca
				d = incidentCrossing db

				nextIncoming = incoming ++ [db]

				pass = makePass nextIncoming

		makePass incoming
			| Maybe.isJust pass  = pass
			| Maybe.isJust self  = self
			| otherwise          = Maybe.Nothing

			where
				testLen (outcoming, _) = 
					if length outcoming >= 0
						then return outcoming
						else Maybe.Nothing

				selfIncoming = map (continuation . opposite) incoming

				testSelf outcoming =
					if everything == List.nub everything
						then return outcoming
						else Maybe.Nothing

					where
						everything = List.sort $ map incidentCrossing $ selfIncoming ++ outcoming

				pass = restingPart tangle incoming >>= testLen >>= (return . performPass tangle incoming)

				self = restingPart tangle selfIncoming >>= testLen >>= testSelf >>= (return . performPass tangle incoming)


performPass :: (Show t, Tangle t c d ArbitraryCrossing) => t -> [d] -> [d] -> (TangleSt.TangleSt ArbitraryCrossing, Int)
performPass tangle incoming outcoming
	| n < m      = error "performPass: bad sizes"
	| otherwise  =
		moveZ tangle $ do
			substituteM $ (map (\ d -> (d, continuation $ opposite d)) incoming) ++ (zip (map opposite incoming) outcoming)
			connectM $ zip outcoming $ map (continuation . opposite) incoming
			when (not $ null toRemove) $ do
				maskM $ map adjacentCrossing toRemove
				let p = nextCCW $ opposite $ last toRemove
				let q = opposite $ nextCW $ opposite $ head toRemove
				substituteM [(q, p)]
			greedy [reduce1st, reduce2nd]

	where
		n = length incoming
		m = length outcoming

		toRemove = drop m incoming
