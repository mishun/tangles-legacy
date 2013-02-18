module Main (
	  main
	) where

import qualified System.CPUTime as CPUTime

import Control.Monad
import Text.Printf

import Math.Combinatorics.ChordDiagrams.Generator


main :: IO ()
main = do
	putStrLn "Chord diagrams generator"
	beginTime <- CPUTime.getCPUTime

	--let getCD !n = execState (generate n (\ _ -> get >>= \ !s -> put $! (s + 1))) 0
	forM_ [0 .. 10] $ \ i -> do
		let cd = generateNonPlanar i (\ c _ -> c `seq` (c + 1)) (0 :: Int)
		putStrLn $ printf "%2i: %i" i cd

	endTime <- CPUTime.getCPUTime
	putStrLn $ printf "Time = %fs" $ ((fromInteger (endTime - beginTime)) :: Float) / 1.0e12
	return ()
