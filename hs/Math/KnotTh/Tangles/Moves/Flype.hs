module Math.KnotTh.Tangles.Moves.Flype
	(
	  neighbours
	) where

import qualified Data.Maybe as Maybe
import qualified Data.Array as Array

import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles.Util.Resting
import Math.KnotTh.Tangles.Moves.Moves


neighbours :: (Show t, Tangle t c d ArbitraryCrossing) => t -> [(TangleSt.TangleSt ArbitraryCrossing, Int)]
neighbours tangle = Maybe.mapMaybe tryFlype $ allDarts tangle
	where
		tryFlype ab
			| isLeg ba || isLeg ca        = Maybe.Nothing
			| b == c || a == b || a == c  = Maybe.Nothing
			| Maybe.isNothing flype2T     = Maybe.Nothing
			| otherwise                   =
				Maybe.Just $ moveZ tangle $ do
					substituteM [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
					connectM [(rp, ae), (sq, ad)]
					flipM $ filter ((Array.!) sub) $ allCrossings tangle

			where
				ba = opposite ab
				ac = nextCCW ab
				ca = opposite ac

				a = incidentCrossing ab
				b = incidentCrossing ba
				c = incidentCrossing ca

				ae = nextCCW ac
				ad = nextCW ab

				flype2T = restingPart tangle [ba, ca] >>= otherPair
					where
						otherPair (lst, s) =
							case lst of
								[x, y] -> Maybe.Just $! ((x, y), s)
								_      -> Maybe.Nothing

				((rp, sq), sub) = Maybe.fromJust flype2T
