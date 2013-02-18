module Math.Combinatorics.ChordDiagrams.Generator
	(
	  generateNonPlanar
	) where

import qualified Data.List as List

import Data.Maybe
import Data.Array.ST
import Data.STRef.Strict
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Strict
import Data.Array.Base

import Algo.Lyndon


generateAction :: Int -> (STUArray s Int Int -> Int -> Int -> ST s Bool) -> Int -> (st -> Int -> st) -> st -> ST s st
generateAction startChordLength checker n yield initial = do
	let p = 2 * n

	a <- newArray (0, p - 1) 0 :: ST s (STUArray s Int Int)
	prev <- newListArray (0, p - 1) (p : [0 .. p - 2]) :: ST s (STUArray s Int Int)
	next <- newListArray (0, p - 1) [1 .. p] :: ST s (STUArray s Int Int)
	begin <- newSTRef 0

	let hide !i = do
		nx <- unsafeRead next i
		pv <- unsafeRead prev i
		when (nx < p) (unsafeWrite prev nx pv)
		if pv < p
			then unsafeWrite next pv nx
			else writeSTRef begin nx

	let restore !i = do
		nx <- unsafeRead next i
		pv <- unsafeRead prev i
		when (nx < p) (unsafeWrite prev nx i)
		if pv < p
			then unsafeWrite next pv i
			else writeSTRef begin i

	let try !u !v action = do
		ok <- checker a u v
		when ok $ do
			unsafeWrite a u (v - u)
			unsafeWrite a v (p + u - v)
			hide u
			hide v
			_ <- action
			restore v
			restore u
			unsafeWrite a u 0
			unsafeWrite a v 0

	result <- newSTRef initial

	let yieldAction cd = do
		state <- readSTRef result
		writeSTRef result $! yield state cd

	let matchRest !minLength = do
		u <- readSTRef begin
		if u < p
			then do
				let match !pv = do
					v <- unsafeRead next pv
					when (v < min p (p + u - minLength + 1)) $ do
						when (v - u >= minLength) $ do
							ok <- checker a u v
							when ok $ do
								unsafeWrite a u (v - u)
								unsafeWrite a v (p + u - v)
								hide u
								hide v
								matchRest minLength
								restore v
								restore u
								unsafeWrite a u 0
								unsafeWrite a v 0
						match v
				match u
			else yieldAction 0

	let backtrack !chordLength !period !mirror = do

		code <- newArray (0, 3 * p) 0 :: ST s (STUArray s Int Int)

		let checkMirror !codeLen !newPeriod = case mirror of
			Nothing         -> backtrack (chordLength + 1) newPeriod Nothing
			Just mirrorBase -> do
				dir <- mapM (unsafeRead code) [0 .. codeLen - 1]

				rev <- do
					let base = mod (mirrorBase + p - chordLength) period
					let start = snd $ fromJust $ List.find ((> base) . fst) $ zip dir (codeLen - 1 : [0 ..])

					tmp <- newSTRef []
					let push !x = do
						t <- readSTRef tmp
						writeSTRef tmp $! x : t

					let iter !i !cross
						| i == codeLen  = when cross (push period)
						| otherwise     = do
							value <- unsafeRead code $ mod (start + codeLen - i) codeLen
							ncross <- if cross && (value <= base || value == period)
								then do
									push period
									return False
								else return $! cross

							if value < period
								then do
									push $ mod (base + period - value) period
									iter (i + 1) ncross
								else iter (i + 1) True


					iter 0 False
					List.reverse <$> readSTRef tmp

				let (shift, minRev) = minimumCyclicShift rev

				case compare dir minRev of
					LT -> backtrack (chordLength + 1) newPeriod Nothing
					EQ -> do
						let periods = length $ filter (== period) $ take shift rev
						let nextBase = mod (mod (mirrorBase + p - chordLength) period + periods * (p - period) + chordLength) p
						backtrack (chordLength + 1) newPeriod (Just $! nextBase)
					GT -> return ()

		let placeChords !pos !lyn !chunk !lower
			| chunk == div p period  = when (mod pos lyn == 0) (checkMirror pos (div (p * lyn) pos))
			| otherwise              = do
				lyndonPrev <- if pos == 0 then return 0 else unsafeRead code (pos - lyn)
				let iteration j = do
					unsafeWrite code pos j
					let nextLyn = if j == lyndonPrev then lyn else pos + 1

					if j < period
						then do
							let u = chunk * period + j;
							let v = mod (u + chordLength) p

							ue <- unsafeRead a u
							ve <- unsafeRead a v
							when (ue == 0 && ve == 0) $ try (min u v) (max u v) $ placeChords (pos + 1) nextLyn chunk (j + 1)

							iteration (j + 1)
						else placeChords (pos + 1) nextLyn (chunk + 1) 0

				let bottom = max lyndonPrev lower
				when (bottom <= period) (iteration bottom)

		full <- (>= p) <$> readSTRef begin
		if full
			then yieldAction 0
			else if ((period == p) && (isNothing mirror)) || (chordLength >= n)
				then matchRest chordLength
				else placeChords 0 1 0 0

	backtrack (max 1 startChordLength) 1 (Just 0)
	readSTRef result


generateNonPlanar :: Int -> (st -> Int -> st) -> st -> st
generateNonPlanar n yield initial = runST $ generateAction 2 (\ _ _ _ -> return True) n yield initial
	