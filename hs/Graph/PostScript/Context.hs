module Graph.PostScript.Context
	(
	  contextToPS
	) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Graph.Color
import Graph.DrawContext


contextToPS :: DrawContext -> String
contextToPS context =
	if List.all null codes
		then ""
		else List.intercalate " " $ filter (not . null) codes

	where
		codes = [make showColor drawColor, make showWidth drawLineWidth, make showDash dashType]

		showColor color =
			let (r, g, b) = toRGB color
			in (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " setrgbcolor"

		showWidth width = (show $ fst width) ++ " setlinewidth"

		showDash dash = case dash of
			DashedEvenly -> List.concat ["[", (show width), " ", (show width), "] 0 setdash"]
			_            -> ""

			where
				width = 4 * (Maybe.maybe 1.0 fst (drawLineWidth context))

		make g f =
			if Maybe.isJust fc
				then res
				else ""

			where
				fc = f context
				res = g $ Maybe.fromJust fc
