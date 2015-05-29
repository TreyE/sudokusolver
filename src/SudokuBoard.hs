module SudokuBoard

where

import Data.List
import Data.Tuple

data Cell = Empty Int [Int] | Entry Int Int
type Board = [Cell]

instance Show Cell where
  show (Empty _ rest) = "_" ++ "(" ++ (show rest)  ++ ")"
  show (Entry _ i) = show i

showCell :: Cell -> String
showCell (Empty _ _) = "_"
showCell (Entry _ i) = show i

showBoard :: Board -> String
showBoard b = foldl' (\x y -> x ++ (showCell y)) "" b

boardComplexity :: Board -> Int
boardComplexity = foldl' howComplexIs 0

howComplexIs :: Int -> Cell -> Int
howComplexIs i (Entry _ _ ) = i
howComplexIs i (Empty _ rest) = i + (length rest)

isComplete :: Cell -> Bool
isComplete (Entry _ _) = True
isComplete _ = False

indexOfCell :: (Int, Int) -> Int
indexOfCell (x,y) = x + (y * 9)

cellLocation :: Int -> (Int, Int)
cellLocation i = swap $ divMod i 9

cellAt :: Board -> (Int, Int) -> Cell
cellAt b i = b !! (indexOfCell i)

cellColumn :: Int -> Int
cellColumn = snd . cellLocation

cellRow :: Int -> Int
cellRow = fst . cellLocation

cellBoard :: Int -> Int
cellBoard i = ((y `div` 3) * 3) + (x `div` 3)
              where (x,y) = cellLocation i

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
