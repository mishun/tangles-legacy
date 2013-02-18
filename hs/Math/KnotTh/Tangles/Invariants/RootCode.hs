module Math.KnotTh.Tangles.Invariants.RootCode
	(
	  rootCodeLeg
	) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Bits as Bits
import qualified System.IO.Unsafe as IOUnsafe
--import qualified Control.Monad.ST as ST
--import qualified Data.STRef as STRef
--import qualified Data.Array.ST as STArr

import Data.Array (listArray, (!), (//))
--import Data.Array.Base (unsafeRead, unsafeWrite)
--import Control.Applicative
--import Control.Monad

import qualified Math.Algebra.RotationDirection as RotationDirection
import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.D4 as D4

import Math.KnotTh
import Math.KnotTh.Tangles

import Foreign
import Foreign.C.Types


rootCodeLeg :: (Tangle t c d ct) => t -> d -> RotationDirection.RotationDirection -> D4.Element -> [Int]
rootCodeLeg tangle leg dir global
	| not (isLeg leg)  = error "rootCodeLeg: leg expected"
	| otherwise        = a --if a == b then a else error $ "fuck! " ++ show a ++ " " ++ show b
	where
		a = rootCode' tangle (opposite leg) dir global
		b = rootCode'' tangle (opposite leg) dir global


rootCode' :: (Tangle t c d ct) => t -> d -> RotationDirection.RotationDirection -> D4.Element -> [Int]
rootCode' tangle start dir globalTransform
	| isLeg start  = error "can not calculate Root-Code from leg"
	| otherwise    = code initial

	where
		nextFun = RotationDirection.pickCWOrCCW (nextCW, nextCCW) dir

		initial = ([start], emptyIds // [(i, Just 1)], 2)
			where
				i = incidentCrossing start

				emptyIds = listArray (crossingsRange tangle) $ List.repeat Nothing

		code ([], _, _) = []
		code st = (identifier cr) : union : code s4
			where
				(d0, s0) = dequeue st
				d1 = nextFun d0
				d2 = nextFun d1
				d3 = nextFun d2

				(i0, s1) = look s0 d0
				(i1, s2) = look s1 d1
				(i2, s3) = look s2 d2
				(i3, s4) = look s3 d3

				(cr, g) = stateRelativeToDart dir d0

				orientation = Group.factorSetId (localSymmetry cr) (g Group.<*> globalTransform)

				union = (Bits.shiftL i0 24) + (Bits.shiftL i1 17) + (Bits.shiftL i2 10) + (Bits.shiftL i3 3) + orientation

		look st@(queue, ids, freeId) drt =
			if isLeg nbDart
				then (0, st)
				else if Maybe.isJust cid
					then (Maybe.fromJust cid, st)
					else (freeId, newState)

			where
				nbDart = opposite drt
				nbCrossing = fst $ begin nbDart

				cid = ids ! nbCrossing
				newState = (queue ++ [nbDart], ids // [(nbCrossing, Just freeId)], freeId + 1)

		dequeue ([], _, _) = error "dequeue from empty queue"
		dequeue (qf : queue, ids, freeId) = (qf, (queue, ids, freeId))


{-rootCode'' :: (Tangle t c d ct) => t -> d -> RotationDirection.RotationDirection -> D4.Element -> [Int]
rootCode'' tangle start dir globalTransform =
	ST.runST $ do
		rcode <- STArr.newArray (0, 2 * n - 1) 0 :: ST.ST s (STArr.STUArray s Int Int)

		qtail <- STRef.newSTRef 1
		queue <- (STArr.newArray (0, n - 1) :: d -> ST.ST s (STArr.STArray s Int d)) start
		unsafeWrite queue 0 start

		idx <- STArr.newArray (0, n) 0 :: ST.ST s (STArr.STUArray s Int Int)
		unsafeWrite idx (index $ incidentCrossing start) 1

		forM_ [0 .. (n - 1)] $ \ qhead -> do
			base <- unsafeRead queue qhead

			neighbourhood <-
				let look !preCode !u = do
					let v = opposite u
					cur <- if isLeg v
						then return 0
						else do
							i <- unsafeRead idx (index $ incidentCrossing v)

							if i /= 0
								then return $! i
								else do
									free <- STRef.readSTRef qtail
									STRef.writeSTRef qtail $! (free + 1)

									unsafeWrite idx (index $! incidentCrossing v) $! (free + 1)
									unsafeWrite queue free v

									return $! (free + 1)

					return $! (Bits.shiftL preCode 7 + cur)

				in do
					let	d0 = base
						d1 = nextFun d0
						d2 = nextFun d1
						d3 = nextFun d2
					foldM look 0 [d0, d1, d2, d3]

			let (cr, g) = stateRelativeToDart dir base
			let orientation = Group.factorSetId (localSymmetry cr) (g Group.<*> globalTransform)

			unsafeWrite rcode (2 * qhead) $! (identifier cr)
			unsafeWrite rcode (2 * qhead + 1) $! (Bits.shiftL neighbourhood 3 + orientation)
			return ()

		mapM (unsafeRead rcode) [0 .. (2 * n - 1)]

	where
		n = numberOfCrossings tangle

		nextFun =
			if dir == RotationDirection.CounterClockwise
				then nextCCW
				else nextCW
-}

foreign import ccall "_ZN4Math6KnotTh7Tangles10Invariants8rootCodeEPiS3_PKiiii"
	c_rootCode :: Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt -> IO ()

rootCode'' :: (Tangle t c d ct) => t -> d -> RotationDirection.RotationDirection -> D4.Element -> [Int]
rootCode'' tangle start direction globalTransform =
	IOUnsafe.unsafePerformIO $! withArray ({-# SCC "connections_c" #-}0 : 0 : 0 : 0 : connList) $! \ connect -> do
		rcode <- {-# SCC "maloc1" #-}mallocArray n
		queue <- mallocArray n

		{-# SCC "c_code" #-} c_rootCode rcode queue connect (fromIntegral n) (dartToConn start) dir

		{-result <- forM [0 .. (n - 1)] $! \ i -> do
			loc <- fromIntegral <$> peek (advancePtr rcode i)
			drt <- fromIntegral <$> peek (advancePtr queue i)

			--let d = dart (drt Bits..&. 3) $! crossing (Bits.shiftR drt 2) tangle
			--let (cr, g) = stateRelativeToDart direction d
			--let orientation = Group.factorSetId (localSymmetry cr) (g Group.<*> globalTransform)

			--return $! [identifier cr, loc + orientation]
			return $! loc-}

		result <- {-# SCC "peek_from_c" #-} peekArray n rcode

		free queue
		free rcode
		return $! {-# SCC "c_fromintegral" #-} map fromIntegral $! result

	where
		n = numberOfCrossings tangle

		dir = case direction of
			RotationDirection.CounterClockwise -> 1
			RotationDirection.Clockwise        -> -1

		connList = map (dartToConn . opposite) $! allDarts tangle

		dartToConn v = {-# SCC "convert_to_c" #-}
			if isLeg v
				then 0
				else fromIntegral $!
					let (cr, place) = begin v
					in Bits.shiftL (index cr) 2 + place
