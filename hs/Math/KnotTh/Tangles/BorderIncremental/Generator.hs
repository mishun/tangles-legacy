module Math.KnotTh.Tangles.BorderIncremental.Generator
	(
	  generate
	, generateFromCrossings
	) where

import qualified Data.Maybe as Maybe

import Control.Monad

import Math.KnotTh
import Math.KnotTh.Tangles.BorderIncremental.BITangle
import Math.KnotTh.Tangles.BorderIncremental.GlueSites


generate :: (CrossingType ct, Monad m)
	=> (BITangle ct -> (Int, BIDart ct) -> [ct]) -> Int -> BITangle ct -> (BITangle ct -> m ()) -> m ()

generate crossingsLister maxN initial yield = dfs initial
	where
		dfs tangle = do
			yield tangle
			when (n < maxN) $ mapM_ dfs children

			where
				n = numberOfCrossings tangle

				children = Maybe.mapMaybe (glue tangle) $ glueSites crossingsLister tangle


generateFromCrossings :: (CrossingType ct, Monad m)
	=> (PreChecker ct, PostChecker ct) -> [ct] -> Int -> (BITangle ct -> m ()) -> m ()

generateFromCrossings checker crs maxN yield =
	let fromRoot r = generate (\ _ _ -> crs) maxN (create checker r) yield
	in mapM_ fromRoot crs
