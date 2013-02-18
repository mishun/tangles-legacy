module Graph.PostScript
	(
	  writePostScriptFile
	, toPostScript
	) where

import qualified Data.List as List
import qualified System.IO as IO

import Graph.DrawContext
import Graph.Image
import Graph.ImageBody
import Graph.Transform
import Graph.PostScript.Transform
import Graph.PostScript.Context
import Graph.PostScript.Path


writePostScriptFile :: String -> Image () -> IO.IO ()
writePostScriptFile fileName img = do
	file <- IO.openFile fileName IO.WriteMode
	mapM_ (IO.hPutStr file) [header, toPostScript img, eof]
	IO.hClose file
	return ()

	where
		header = "%!PS-Adobe-2.0\n%%EndComments\n\n"
		eof = "%%Trailer\n%%EOF\n"


toPostScript :: Image () -> String
toPostScript (Image img ()) = traverse gen img
	where
		gen = CodeGenerator strokeCode fillCode

		strokeCode (gc, lc) (gt, lt) path = psWithContext (aggregateContexts [gc, lc], transform [gt, lt]) $ (pathToPS path) ++ " stroke\n"

		fillCode (gc, lc) (gt, lt) path = psWithContext (aggregateContexts [gc, lc], transform [gt, lt]) $ (pathToPS path) ++ " fill\n"


psWithContext :: (DrawContext, Transform) -> String -> String
psWithContext (con, trans) psCode
	| null psCode       = ""
	| null contextCode  = psCode ++ "\n"
	| otherwise         = List.concat ["gsave\n", contextCode, "\n", psCode, "grestore\n"]

	where
		contextCode = List.intercalate "\n" $ filter (not . null) [contextToPS con, transformToPS trans]
