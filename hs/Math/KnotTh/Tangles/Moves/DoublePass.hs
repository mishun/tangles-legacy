module Math.KnotTh.Tangles.Moves.DoublePass
	(
	  neighbours
	) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

--import Control.Monad

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles.Util.Resting
--import Math.KnotTh.Tangles.Moves.Moves
--import Math.KnotTh.Tangles.Moves.ReidemeisterReduction

import Debug.Trace


neighbours :: (Show t, Tangle t c d ArbitraryCrossing) => t -> [(TangleSt.TangleSt ArbitraryCrossing, Int)]
neighbours tangle = Maybe.mapMaybe tryDoublePass $ allDarts tangle
	where
		tryDoublePass ab
			| unexpectedLeg             = Maybe.Nothing
			| List.any isLeg incomingA  = Maybe.Nothing
			| List.any isLeg incomingB  = Maybe.Nothing
			| Maybe.isNothing maybeA    = Maybe.Nothing
			| Maybe.isNothing maybeB    = Maybe.Nothing
		--	| length outcomingA /= 2    = Maybe.Nothing
		--	| length outcomingB /= 2    = Maybe.Nothing
			| otherwise                 = trace "found" Maybe.Nothing

			where
				ba = opposite ab
				bc = nextCCW ba
				cb = opposite bc
				cd = nextCW cb
				dc = opposite cd

				unexpectedLeg = isLeg ba || isLeg cb || isLeg dc

				incomingA = map opposite [continuation ab, nextCW ab, nextCCW cb, continuation cb]
				incomingB = map opposite [continuation dc, nextCW dc, nextCCW bc, continuation bc]

				maybeA = restingPart tangle incomingA
				maybeB = restingPart tangle incomingB

				outcomingA = fst $ Maybe.fromJust maybeA
				outcomingB = fst $ Maybe.fromJust maybeB
