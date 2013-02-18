module Math.KnotTh.Tangles.Moves.Moves
	(
	  MoveM

	, move
	, moveZ
	, greedy
	, connectM
	, substituteM
	, maskM
	, flipM
	, oppositeM
	, emitCircleM
	) where

import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.IORef as IORef
import qualified System.IO.Unsafe as IOUnsafe
import qualified Control.Monad.State.Strict as State

import Control.Monad
import Control.Applicative

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.TangleSt


data MutableDartArray t c d ct a = MDA !(IOArray.IOArray d a) !(IOArray.IOArray d a)

type MoveM t c d ct a = State.StateT (MoveState t c d ct) IO a


createMDA :: (Tangle t c d ct) => t -> (d -> a) -> IO (MutableDartArray t c d ct a)
createMDA tangle f = do
	dartsArr <- IOArray.newListArray (dartsRange tangle) $ map f darts
	legsArr <- IOArray.newListArray (legsRange tangle) $ map f legs
	return $! MDA dartsArr legsArr

	where
		darts = allDarts tangle
		legs = allLegs tangle


modifyMDA :: (Tangle t c d ct) => MutableDartArray t c d ct a -> [(d, a)] -> IO ()
modifyMDA (MDA dartsArr legsArr) list =
	forM_ list $ (\ (i, v) -> IOArray.writeArray (if isLeg i then legsArr else dartsArr) i v)


readMDA :: (Tangle t c d ct) => MutableDartArray t c d ct a -> d -> IO a
readMDA (MDA dartsArr legsArr) i = IOArray.readArray (if isLeg i then legsArr else dartsArr) i


extractMDAState :: (Tangle t c d ct) => MutableDartArray t c d ct a -> IO (d -> a)
extractMDAState (MDA dartsArr legsArr) = do
	dartsFreeze <- freeze dartsArr
	legsFreeze <- freeze legsArr
	return $! (\ d -> (if isLeg d then legsFreeze else dartsFreeze) Array.! d)

	where
		freeze :: (Array.Ix d) => IOArray.IOArray d a -> IO (Array.Array d a)
		freeze = IOArray.freeze



data CrossingFlag = Direct | Flipped | Masked deriving (Eq, Enum)

data MoveState t c d ct = MvSt
	{
		stateSource :: !t,
		stateConnections :: !(MutableDartArray t c d ct d),
		stateMask :: !(IOArray.IOArray c CrossingFlag),
		stateCircles :: !(IORef.IORef Int)
	}


disassemble :: (Tangle t c d ct) => (t, Int) -> IO (MoveState t c d ct)
disassemble (tangle, circles) = do
	connections <- createMDA tangle opposite
	msk <- IOArray.newArray (crossingsRange tangle) Direct
	circlesCounter <- IORef.newIORef circles
	return $! MvSt tangle connections msk circlesCounter


assemble :: (Tangle t c d ct) => MoveState t c d ct -> IO (TangleSt ct, Int)
assemble st = do
	pair <- do
		idxList <- scanl (\ i f -> if f == Masked then i else i + 1) 1 <$> (IOArray.getElems $ stateMask st)
		let idx = Array.listArray (crossingsRange source) idxList
		msk <- freeze (stateMask st)

		return $! \ !d ->
			if isLeg d
				then (0 :: Int, legPosition d)
				else	let (c, place) = begin d
					in case msk Array.! c of
						Direct  -> (idx Array.! c, place)
						Flipped -> (idx Array.! c, (3 - place) Bits..&. 3)
						Masked  -> error "assemble: using masked crossing"

	left <-	let el c = do
			m <- IOArray.readArray (stateMask st) c
			return (c, m)
		in filter ((/= Masked) . snd) <$> (mapM el $ allCrossings source)

	opp <- extractMDAState $ stateConnections st

	let result = constructFromList (border : internal, map (state . fst) left)
		where
			border = map (pair . opp) $ allLegs source

			internal = map (map (pair . opp) . surroundings) left
				where
					surroundings (!c, !m) = case m of
						Direct  -> incidentDarts c
						Flipped -> List.reverse $ incidentDarts c
						_       -> error "wtf"

	circles <- IORef.readIORef (stateCircles st)

	return $! (result, circles)

	where
		source = stateSource st

		freeze :: (Array.Ix d) => IOArray.IOArray d a -> IO (Array.Array d a)
		freeze = IOArray.freeze


reconnect :: (Tangle t c d ct) => MoveState t c d ct -> [(d, d)] -> IO ()
reconnect st = modifyMDA (stateConnections st) . List.concatMap (\ (a, b) -> [(a, b), (b, a)])



move :: (Tangle t c d ct) => (t, Int) -> State.StateT (MoveState t c d ct) IO () -> (TangleSt ct, Int)
move tangleS modification =
	IOUnsafe.unsafePerformIO $
		disassemble tangleS >>= State.execStateT modification >>= assemble


moveZ :: (Tangle t c d ct) => t -> State.StateT (MoveState t c d ct) IO () -> (TangleSt ct, Int)
moveZ tangle = move (tangle, 0)


greedy :: (Tangle t c d ct) => [d -> State.StateT (MoveState t c d ct) IO Bool] -> State.StateT (MoveState t c d ct) IO ()
greedy reductionsList = iteration
	where
		iteration = do
			darts <- State.get >>= \ st -> State.lift $ return $ allDarts $ stateSource st
			changed <- anyM processDart darts
			when changed iteration

		processDart d = do
			masked <- State.get >>= \ st -> State.lift $ IOArray.readArray (stateMask st) (fst $ begin d)
			if masked == Masked
				then return False
				else anyM (\ r -> r d) reductionsList

		anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
		anyM _ [] = return False
		anyM f (cur : rest) = do
			res <- f cur
			if res
				then return True
				else anyM f rest


connectM :: (Tangle t c d ct) => [(d, d)] -> State.StateT (MoveState t c d ct) IO ()
connectM connections =
	State.get >>= \ st -> State.lift $
		reconnect st connections


substituteM :: (Tangle t c d ct) => [(d, d)] -> State.StateT (MoveState t c d ct) IO ()
substituteM substitutions = do
	reconnections <- mapM (\ (a, b) -> do { ob <- oppositeM b ; return $! (a, ob) }) substitutions
	st <- State.get
	State.lift $ do
		aux <- do
			let testSubsts (a, b) =
				if a == b
					then do
						IORef.modifyIORef (stateCircles st) (+ 1)
						return False
					else return True

			arr <- createMDA (stateSource st) id
			assignments <- filterM testSubsts substitutions
			modifyMDA arr $ map swap assignments
			extractMDAState arr

		reconnect st $ map (\ (a, b) -> (a, aux b)) reconnections

	where
		swap (a, b) = (b, a)


maskM :: (Tangle t c d ct) => [c] -> State.StateT (MoveState t c d ct) IO ()
maskM crossings =
	State.get >>= (\ st -> State.lift $ forM_ crossings $ (\ c -> IOArray.writeArray (stateMask st) c Masked))


flipM :: (Tangle t c d ct) => [c] -> State.StateT (MoveState t c d ct) IO ()
flipM crossings =
	State.get >>= \ st -> State.lift $ forM_ crossings $ \ c -> do
		msk <- IOArray.readArray (stateMask st) c
		IOArray.writeArray (stateMask st) c $
			case msk of
				Direct  -> Flipped
				Flipped -> Direct
				Masked  -> error "flipM: flipping masked crossing"


oppositeM :: (Tangle t c d ct) => d -> State.StateT (MoveState t c d ct) IO d
oppositeM d = State.get >>= \ st -> State.lift $ readMDA (stateConnections st) d


emitCircleM :: (Tangle t c d ct) => State.StateT (MoveState t c d ct) IO ()
emitCircleM = State.get >>= \ st -> State.lift $ IORef.modifyIORef (stateCircles st) (+ 1)
