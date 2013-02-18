module Math.KnotTh.Tangles.Moves.Weak
	(
	  neighbours
	) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles.Moves.Moves


neighbours :: (Tangle t c d ArbitraryCrossing) => t -> [(TangleSt.TangleSt ArbitraryCrossing, Int)]
neighbours tangle = List.concatMap (\ f -> f tangle) [neighboursBorderCrossing, neighboursBorderLoop]


neighboursBorderCrossing :: (Tangle t c d ArbitraryCrossing) => t -> [(TangleSt.TangleSt ArbitraryCrossing, Int)]
neighboursBorderCrossing tangle = Maybe.mapMaybe tryReduceLeg $ allLegs tangle
	where
		tryReduceLeg xa
			| isLeg ax           = Maybe.Nothing
			| ya /= opposite ay  = Maybe.Nothing
			| otherwise          =
				Maybe.Just $ moveZ tangle $ do
					maskM [a]
					if qa == ap
						then do
							connectM [(xa, ya)]
							emitCircleM
						else connectM [(pa, ya), (qa, xa)]

			where
				ax = opposite xa

				ay = nextCCW ax
				ya = nextCCW xa

				a = incidentCrossing ax

				ap = nextCCW ay
				aq = nextCCW ap

				pa = opposite ap
				qa = opposite aq


neighboursBorderLoop :: (Tangle t c d ArbitraryCrossing) => t -> [(TangleSt.TangleSt ArbitraryCrossing, Int)]
neighboursBorderLoop tangle = Maybe.mapMaybe tryReduceLoop $ allLegs tangle
	where
		tryReduceLoop xa
			| isLeg ax                    = Maybe.Nothing
			| isLeg bar                   = Maybe.Nothing
			| yb /= opposite by           = Maybe.Nothing
			| abl /= opposite bal         = Maybe.Nothing
			| passOver ax == passOver by  = Maybe.Nothing
			| otherwise                   =
				Maybe.Just $ moveZ tangle $ do
					substituteM [(abl, ap), (bal, bq)]
					connectM [(ax, by), (ap, xa), (bq, yb)]

			where
				ax = opposite xa
				abr = nextCCW ax
				abl = nextCCW abr
				ap = nextCW ax

				bar = opposite abr
				bal = nextCW bar
				by = nextCCW bar
				bq = nextCCW by

				yb = nextCCW xa
