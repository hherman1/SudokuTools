module Sudoku (
	Piece (..),
	Board,
) where
import Input

data Piece = Known Int | Unknown [Int] deriving (Show, Eq)

type Board = [[Piece]]


