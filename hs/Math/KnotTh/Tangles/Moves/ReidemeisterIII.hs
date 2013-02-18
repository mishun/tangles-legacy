module Math.KnotTh.Tangles.Moves.ReidemeisterIII
	(
	  neighbours
	) where

import qualified Data.Maybe as Maybe

import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles.Moves.Moves


neighbours :: (Tangle t c d ArbitraryCrossing) => t -> [(TangleSt.TangleSt ArbitraryCrossing, Int)]
neighbours tangle = Maybe.mapMaybe try3rdReidemeister $ allDarts tangle
	where
		-- \sc           /rb             \sc   /rb
		--  \           /                 \   /
		-- cs\ cb   bc /br               ac\ /ab
		-- ---------------                  /
		--   ca\c   b/ba                 ap/a\aq
		--      \   /         -->         /   \
		--     ac\ /ab                 cs/c   b\br
		--        /                  ---------------
		--     ap/a\aq               ca/ cb   bc \ba
		--      /   \                 /           \
		--   pa/     \qa             /pa           \qa
		try3rdReidemeister ab
			| isLeg ba || isLeg ca  = Maybe.Nothing
			| bc /= opposite cb     = Maybe.Nothing
			| not abcAreDifferent   = Maybe.Nothing
			| not threadMovable     = Maybe.Nothing
			| not isBetterRoot      = Maybe.Nothing
			| otherwise             =
				Maybe.Just $ moveZ tangle $ do
					substituteM [(ca, ap), (ba, aq), (ab, br), (ac, cs)]
					connectM [(br, aq), (cs, ap)]

			where
				ac = nextCCW ab

				ba = opposite ab
				ca = opposite ac

				bc = nextCW ba
				cb = nextCCW ca

				a = incidentCrossing ab
				b = incidentCrossing ba
				c = incidentCrossing ca

				abcAreDifferent = (a /= b) && (a /= c) && (b /= c)

				threadMovable = (passOver bc) == (passOver cb)

				isBetterRoot = ab < altRoot
					where
						altRoot = if (passOver ab) == (passOver ba)
							then ca
							else bc

				ap = continuation ab
				aq = nextCW ab
				br = nextCW bc
				cs = nextCCW cb
