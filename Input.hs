module Input (
	showPiece,
	readPiece,
	showBoard,
	readKnownChar,
	inputBoard,
) where

showPiece :: Piece -> String
showPiece (Known i) = "[" ++ show i ++ "]"
showPiece (Unknown _) = "[?]"

readPiece :: String -> Piece
readPiece x | x == "[?]" = Unknown []
readPiece (ob:n:cb:_) | ob == '[' && cb == ']' = Known $ read [n]
readPiece _ = Unknown []

readKnownChar :: Char -> Piece
readKnownChar = Known . read . (:[])

intersperse3 e (a:b:c:es) = a:b:c:e: intersperse3 e es
intersperse3 _ x = x

boardWidth = 30
dashedLine = take boardWidth $ repeat '-'
addNewLine = (++"\n")

showBoard :: Board -> String
showBoard rs = concat $ intersperse3 (addNewLine dashedLine) . (flip map) rs $ addNewLine . showRow
	where
		showRow = concat . intersperse3 "|" . map showPiece

inputBoardAlt :: IO Board
inputBoardAlt = do
	b <-  sequence . take 9 . repeat . sequence . take 9 $ repeat getPiece
	putStr $ showBoard b
	return b

inputBoard :: IO Board
inputBoard = do
		fmap concat . sequence . take 3 . repeat
		. constM (putStr . addNewLine $ dashedLine) . sequence . take 3 . repeat
		. fmap concat . constM (putStr "\n") . sequence . take 3 . repeat
		. constM divide .  sequence . take 3 $ repeat inputPiece
	where
		constM a = (>>=(\x -> a >> return x))
		divide = putChar '|'
		inputPiece = do 
			putChar '[' 
			r <- getPiece 
			putChar ']'
			return r
	
getPiece :: IO Piece
getPiece = fmap readKnownChar getChar

emptyBoard :: Board
emptyBoard = nineOf . nineOf $ Unknown []
	where nineOf =  take 9 . repeat

main :: IO ()
main = putStrLn "test"

