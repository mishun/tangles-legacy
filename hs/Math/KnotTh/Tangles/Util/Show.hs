module Math.KnotTh.Tangles.Util.Show
	(
	  showDart
	, showTangleStructure
	) where

import qualified Data.List as List

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Util.List


showDart :: (Tangle t c d ct) => d -> String
showDart d = List.intercalate " " [show $ toPair d, "->", show $ toPair $ opposite d]


showTangleStructure :: (Tangle t c d ct, Show ct) => t -> String
showTangleStructure tangle = List.intercalate "\n" $ ("O -> " ++ (showLst border)) : (map showCrossing crs)
	where
		showLst lst = "{ " ++ (List.intercalate ", " $ map showEl lst) ++ " }"
			where
				showEl (a, b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"

		(border : crsNb, crsSt) = toList tangle

		crs = zip3 [(1 :: Int) ..] crsNb crsSt

		showCrossing (i, nb, (stc, str)) = (show i) ++ " -> " ++ (showLst nb) ++ showSt
			where
				showSt = " (" ++ (show stc) ++ ", " ++ (show str) ++ ")"
