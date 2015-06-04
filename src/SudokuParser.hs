module SudokuParser(
  parse
) where

import Sudoku.Board
import Data.Char(isDigit, digitToInt)
import Data.List
import qualified Data.Vector as V

parse :: String -> Maybe Board
parse = constructBoard . cleanInput

filterFun :: Char -> Bool
filterFun x = (isDigit x) || (x == '_')

cleanInput :: String -> String
cleanInput = filter (filterFun)

constructBoard :: String -> Maybe Board
constructBoard x | (length x) == 81 = Just $ UnsolvedBoard (V.fromList $ toCells x)
                 | otherwise = Nothing

toCells :: String -> [Cell]
toCells x = snd $ mapAccumL mkCell 0 x

mkCell :: Int -> Char -> (Int, Cell)
mkCell i x | isDigit x = (i + 1, (Entry i (digitToInt x)))
           | otherwise = (i + 1, (Empty i [1..9]))
