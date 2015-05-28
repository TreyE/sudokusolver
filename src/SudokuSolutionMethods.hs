module SudokuSolutionMethods
 where

import SudokuBoard
import Data.List

simplifyByElimination :: Cell -> Cell -> Cell
simplifyByElimination en@(Entry _ _) _ = en
simplifyByElimination en@(Empty _ _) (Empty _ _) = en
simplifyByElimination en@(Empty idx vals) (Entry _ val) = Empty idx (delete val vals)

matchableCellsFor :: Int -> Board -> [Cell]
matchableCellsFor idx = sliceCells ((rowIndexRange (cellRow idx)) ++ (columnIndexRange (cellColumn idx)) ++ (boardRanges (cellBoard idx)))

simpleReduceCell :: Board -> Cell -> Cell
simpleReduceCell b en@(Entry _ _) = en
simpleReduceCell b en@(Empty idx vals) = lastReduce $ foldl' (simplifyByElimination) en (matchableCellsFor idx b)

lastReduce :: Cell -> Cell
lastReduce en@(Entry _ _) = en
lastReduce en@(Empty idx []) = en
lastReduce (Empty idx [x]) = Entry idx x
lastReduce (Empty idx xs) = Empty idx xs

exclusionPass :: Board -> Board
exclusionPass b = map (\c -> simpleReduceCell b c) b
