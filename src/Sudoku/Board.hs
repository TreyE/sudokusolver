module Sudoku.Board

where

import Data.List
import Data.Tuple

data Cell = Empty Int [Int] | Entry Int Int
data Board = SolvedBoard [Cell]
             | UnsolvedBoard [Cell]

instance Show Board where
  show = boardToString

endOfLineCell :: Cell -> Bool
endOfLineCell (Empty i _) = i `mod` 9 == 8
endOfLineCell (Entry i _) = i `mod` 9 == 8

boardToString :: Board -> String
boardToString (SolvedBoard cs) = foldl' showCurrentCell "\n" cs
boardToString (UnsolvedBoard cs) = foldl' showCurrentCell "\n" cs

showCurrentCell :: String -> Cell -> String
showCurrentCell s c | endOfLineCell c = s ++ (show c) ++ "\n"
                    | otherwise = s ++ (show c) ++ " "

boardComplete :: Board -> Bool
boardComplete (SolvedBoard _) = True
boardComplete _ = False

maybeSolve :: Board -> Board
maybeSolve b@(SolvedBoard _) = b
maybeSolve usb@(UnsolvedBoard c) | (all isComplete c) = SolvedBoard c
    | otherwise = usb

splitOnGuess :: [Cell] -> [[Cell]]
splitOnGuess b = splitOnGuess' b []

splitOnGuess' [] bHead = [bHead]
splitOnGuess' ((Empty i emps):xs) bHead = map (\e -> bHead ++ ((Entry i e ):xs)) emps
splitOnGuess' (x:xs) bHead = splitOnGuess' xs (bHead ++ [x])

instance Show Cell where
  show (Empty _ rest) = "_" ++ "(" ++ (show rest)  ++ ")"
  show (Entry _ i) = show i

validCell :: Cell -> Bool
validCell (Empty _ []) = False
validCell _ = True

validBoard :: [Cell] -> Bool
validBoard b = and (map validCell b)

showCell :: Cell -> String
showCell (Empty _ _) = "_"
showCell (Entry _ i) = show i

showBoard :: [Cell] -> String
showBoard b = foldl' (\x y -> x ++ (showCell y)) "" b

boardComplexity :: Board -> Int
boardComplexity (SolvedBoard _) = 0
boardComplexity (UnsolvedBoard b) = foldl' howComplexIs 0 b

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

cellAt :: [Cell] -> (Int, Int) -> Cell
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

sliceCells :: [Int] -> [Cell] -> [Cell]
sliceCells i b = map (b !!) i

row :: Int -> [Cell] -> [Cell]
row = sliceCells . rowIndexRange

column :: Int -> [Cell] -> [Cell]
column = sliceCells . columnIndexRange

subBoard :: Int -> [Cell] -> [Cell]
subBoard = sliceCells . boardRanges
