module Sudoku (
	Piece (..),
	Board,
	isKnown,
) where

data Piece = Known Int | Unknown [Int] deriving (Show, Eq)

type Board = [[Piece]]

isKnown :: Piece -> Bool
isKnown (Known _) = True
isKnown _ = False
