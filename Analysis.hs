module Analysis ()
where

import Board
import Sudoku

data AnalysisStrategy = BoardAnalysis [Board -> Board]

-- Conglomerate function integrating all the different methods of analysis
analyzeBoard

hints :: [Piece] -> [Int]

exclude :: [a] -> [a] -> [a]
exclude exclusions = filter (not . (flip elem) exclusions)

complement = flip exclude

