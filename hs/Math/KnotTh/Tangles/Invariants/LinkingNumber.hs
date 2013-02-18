module Math.KnotTh.Tangles.Invariants.LinkingNumber
	(
	  linkingNumber
	) where

import qualified Data.List as List
import qualified Data.Array as Array
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as STArr

import Control.Monad

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles.Util.Paths


linkingNumber :: (Tangle t c d ArbitraryCrossing) => t -> [Int]
linkingNumber tangle = List.sort $ map abs $ List.concatMap threadLinkings threads
	where
		threads = zip (allThreads tangle) [1 ..]

		n = length threads

		threadId = Array.array (dartsRange tangle) $ z ++ List.concatMap threadNum threads
			where
				z = zip (allDarts tangle) (List.repeat (0 :: Int))

				threadNum (thread, i) = zip (filter isDart $ map snd thread) (List.repeat i)

		threadLinkings (thread, i) =
			ST.runST $ do
				ln <- STArr.newArray (1, n) 0 :: ST.ST s (STArr.STArray s Int Int)

				forM_ (filter isDart $ map snd thread) $ \ d -> do
					let (dl, j) = linking d
					when (i /= j) $ do
						cl <- STArr.readArray ln j
						STArr.writeArray ln j (cl + dl)

				mapM (STArr.readArray ln) [1 .. (i - 1)]

			where
				linking d
					| l > 0             = (p, l)
					| r > 0             = (-p, r)
					| otherwise         = error "no thread"

					where
						p = if passOver d
							then 1
							else -1

						l = (Array.!) threadId (nextCCW d)

						r = (Array.!) threadId (nextCW d)
