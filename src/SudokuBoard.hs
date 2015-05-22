module SudokuBoard

where

import Data.List
import Data.Tuple

data Cell = Empty Int [Int] | Entry Int Int
type Board = [Cell]

indexOfCell :: (Int, Int) -> Int
indexOfCell (x,y) = x + (y * 9)

cellLocation :: Int -> (Int, Int)
cellLocation i = swap $ divMod i 9

cellAt :: Board -> (Int, Int) -> Cell
cellAt b i = b !! (indexOfCell i)

boardRanges :: Int -> [Int]
boardRanges i = [(firstbox + x + (y * 9)) | y <- [0..2],  x <- [0..2]]
                where
                firstbox = ((i `mod` 3) * 3) + ((i `div` 3) * 27)

rowIndexRange :: Int -> [Int]
rowIndexRange i = map indexOfCell [(i, j) | j <- [0..8]]

columnIndexRange :: Int -> [Int]
columnIndexRange i = map indexOfCell [(j, i) | j <- [0..8]]

sliceCells :: [Int] -> Board -> [Cell]
sliceCells i b = map (b !!) i

row :: Int -> Board -> [Cell]
row = sliceCells . rowIndexRange

column :: Int -> Board -> [Cell]
column = sliceCells . columnIndexRange

subBoard :: Int -> Board -> [Cell]
subBoard = sliceCells . boardRanges

simplifyByElimination :: Cell -> Cell -> Cell
simplifyByElimination en@(Entry _ _) _ = en
simplifyByElimination en@(Empty _ _) (Empty _ _) = en
simplifyByElimination en@(Empty idx vals) (Entry _ val) = Empty idx (delete val vals)

matchableCellsFor :: Int -> Board -> [Cell]
matchableCellsFor idx = sliceCells ((rowIndexRange idx) ++ (columnIndexRange idx) ++ (boardRanges idx))

simpleReduceCell :: Board -> Cell -> Cell
simpleReduceCell b en@(Entry _ _) = en
simpleReduceCell b en@(Empty idx vals) = lastReduce $ foldl' (simplifyByElimination) en (matchableCellsFor idx b)

lastReduce :: Cell -> Cell
lastReduce en@(Entry _ _) = en
lastReduce en@(Empty idx []) = en
lastReduce (Empty idx [x]) = Entry idx x
lastReduce (Empty idx xs) = Empty idx xs
