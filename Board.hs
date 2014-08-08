module Board (
	Board,
	Piece,
	emptyBoard,
)
where
import Sudoku

type Boxes = [[Piece]]

toBoxes :: Board -> Boxes
toBoxes board = let boxRows =  as3s $ map as3s board in 
	concat $ map (zipWithInternal3 toList3) boxRows 
	where
		as3s :: [a] -> [[a]]
		as3s (a:b:c:rem) = [a,b,c] : as3s rem
		as3s [] = []
		toList3 a b c = concat [a,b,c]
		zipWithInternal3 f [(a:as),(b:bs),(c:cs)] = f a b c : zipWithInternal3 f [as,bs,cs]
		zipWithInternal3 f _ = []

emptyBoard :: Board
emptyBoard = nineOf . nineOf $ Unknown []
	where nineOf =  take 9 . repeat

testBoard :: Board
testBoard = [[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9],
	[Known 1,Known 2,Known 3,Known 4,Known 5,Known 6,Known 7,Known 8,Known 9]]
