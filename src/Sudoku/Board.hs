module Sudoku.Board
(Cell(Empty,Entry),Board(SolvedBoard,UnsolvedBoard),maybeSolve,cellColumn,cellRow,
cellBoard,boardComplete,splitOnGuess,boardComplexity,sliceCells,cellPos, matchableCellsFor, validBoard)
where

import Data.List
import Data.Tuple
import qualified Data.Vector as V

data Cell = Empty Int [Int] | Entry Int Int
data Board = SolvedBoard (V.Vector Cell)
             | UnsolvedBoard (V.Vector Cell)

instance Show Board where
  show = boardToString

cellPos :: Cell -> Int
cellPos (Empty i _) = i
cellPos (Entry i _) = i

endOfLineCell :: Cell -> Bool
endOfLineCell (Empty i _) = i `mod` 9 == 8
endOfLineCell (Entry i _) = i `mod` 9 == 8

boardToString :: Board -> String
boardToString (SolvedBoard cs) = V.foldl' showCurrentCell "\n" cs
boardToString (UnsolvedBoard cs) = V.foldl' showCurrentCell "\n" cs

showCurrentCell :: String -> Cell -> String
showCurrentCell s c | endOfLineCell c = s ++ (show c) ++ "\n"
                    | otherwise = s ++ (show c) ++ " "

boardComplete :: Board -> Bool
boardComplete (SolvedBoard _) = True
boardComplete _ = False

maybeSolve :: Board -> Board
maybeSolve b@(SolvedBoard _) = b
maybeSolve usb@(UnsolvedBoard c) | (V.all isComplete c) = SolvedBoard c
    | otherwise = usb

splitOnGuess :: V.Vector Cell -> Int -> [[Cell]]
splitOnGuess b i = map (\x -> h ++ ((Entry i x):t)) emps
                     where (Empty _ emps) = b V.! i
                           h = V.toList (V.take i b)
                           t = V.toList (V.drop (i + 1) b)

instance Eq Cell where
  (Entry _ _) == (Empty _ _) = False
  (Empty _ _) == (Entry _ _) = False
  (Entry ia a) == (Entry ib b) = (a == b) && (ia == ib)
  (Empty ia a) == (Empty ib b) = (a == b) && (ia == ib)

instance Ord Cell where
  (Entry ia a) <= (Entry ib b) = ia <= ib
  (Empty _ _) <= (Entry _ _) = True
  (Entry _ _) <= (Empty _ _) = False
  (Empty ia a) <= (Empty ib b) | (length a) == (length b) = ia <= ib
                               | otherwise = (length a) <= (length b)

instance Show Cell where
  show (Empty _ rest) = "_" ++ "(" ++ (show rest)  ++ ")"
  show (Entry _ i) = show i

contentMatch :: Cell -> Cell -> Bool
contentMatch (Empty _ _) _ = False
contentMatch _ (Empty _ _) = False
contentMatch (Entry _ a) (Entry _ b) = (a == b)

validCell :: V.Vector Cell -> Cell -> Bool
validCell _ (Empty _ []) = False
validCell bcs c@(Entry i v) = not (or (V.map (contentMatch c) (matchableCellsFor i bcs)))
validCell _ _ = True

validBoard :: Board -> Bool
validBoard (SolvedBoard b) = and (V.map (validCell b) b)
validBoard (UnsolvedBoard b) = and (V.map (validCell b) b)

matchableCellIndexes :: Int -> [Int]
matchableCellIndexes = (!!) [[1,2,3,4,5,6,7,8,9,10,11,18,19,20,27,36,45,54,63,72],[0,2,3,4,5,6,7,8,9,10,11,18,19,20,28,37,46,55,64,73],[0,1,3,4,5,6,7,8,9,10,11,18,19,20,29,38,47,56,65,74],[0,1,2,4,5,6,7,8,12,13,14,21,22,23,30,39,48,57,66,75],[0,1,2,3,5,6,7,8,12,13,14,21,22,23,31,40,49,58,67,76],[0,1,2,3,4,6,7,8,12,13,14,21,22,23,32,41,50,59,68,77],[0,1,2,3,4,5,7,8,15,16,17,24,25,26,33,42,51,60,69,78],[0,1,2,3,4,5,6,8,15,16,17,24,25,26,34,43,52,61,70,79],[0,1,2,3,4,5,6,7,15,16,17,24,25,26,35,44,53,62,71,80],[0,1,2,10,11,12,13,14,15,16,17,18,19,20,27,36,45,54,63,72],[0,1,2,9,11,12,13,14,15,16,17,18,19,20,28,37,46,55,64,73],[0,1,2,9,10,12,13,14,15,16,17,18,19,20,29,38,47,56,65,74],[3,4,5,9,10,11,13,14,15,16,17,21,22,23,30,39,48,57,66,75],[3,4,5,9,10,11,12,14,15,16,17,21,22,23,31,40,49,58,67,76],[3,4,5,9,10,11,12,13,15,16,17,21,22,23,32,41,50,59,68,77],[6,7,8,9,10,11,12,13,14,16,17,24,25,26,33,42,51,60,69,78],[6,7,8,9,10,11,12,13,14,15,17,24,25,26,34,43,52,61,70,79],[6,7,8,9,10,11,12,13,14,15,16,24,25,26,35,44,53,62,71,80],[0,1,2,9,10,11,19,20,21,22,23,24,25,26,27,36,45,54,63,72],[0,1,2,9,10,11,18,20,21,22,23,24,25,26,28,37,46,55,64,73],[0,1,2,9,10,11,18,19,21,22,23,24,25,26,29,38,47,56,65,74],[3,4,5,12,13,14,18,19,20,22,23,24,25,26,30,39,48,57,66,75],[3,4,5,12,13,14,18,19,20,21,23,24,25,26,31,40,49,58,67,76],[3,4,5,12,13,14,18,19,20,21,22,24,25,26,32,41,50,59,68,77],[6,7,8,15,16,17,18,19,20,21,22,23,25,26,33,42,51,60,69,78],[6,7,8,15,16,17,18,19,20,21,22,23,24,26,34,43,52,61,70,79],[6,7,8,15,16,17,18,19,20,21,22,23,24,25,35,44,53,62,71,80],[0,9,18,28,29,30,31,32,33,34,35,36,37,38,45,46,47,54,63,72],[1,10,19,27,29,30,31,32,33,34,35,36,37,38,45,46,47,55,64,73],[2,11,20,27,28,30,31,32,33,34,35,36,37,38,45,46,47,56,65,74],[3,12,21,27,28,29,31,32,33,34,35,39,40,41,48,49,50,57,66,75],[4,13,22,27,28,29,30,32,33,34,35,39,40,41,48,49,50,58,67,76],[5,14,23,27,28,29,30,31,33,34,35,39,40,41,48,49,50,59,68,77],[6,15,24,27,28,29,30,31,32,34,35,42,43,44,51,52,53,60,69,78],[7,16,25,27,28,29,30,31,32,33,35,42,43,44,51,52,53,61,70,79],[8,17,26,27,28,29,30,31,32,33,34,42,43,44,51,52,53,62,71,80],[0,9,18,27,28,29,37,38,39,40,41,42,43,44,45,46,47,54,63,72],[1,10,19,27,28,29,36,38,39,40,41,42,43,44,45,46,47,55,64,73],[2,11,20,27,28,29,36,37,39,40,41,42,43,44,45,46,47,56,65,74],[3,12,21,30,31,32,36,37,38,40,41,42,43,44,48,49,50,57,66,75],[4,13,22,30,31,32,36,37,38,39,41,42,43,44,48,49,50,58,67,76],[5,14,23,30,31,32,36,37,38,39,40,42,43,44,48,49,50,59,68,77],[6,15,24,33,34,35,36,37,38,39,40,41,43,44,51,52,53,60,69,78],[7,16,25,33,34,35,36,37,38,39,40,41,42,44,51,52,53,61,70,79],[8,17,26,33,34,35,36,37,38,39,40,41,42,43,51,52,53,62,71,80],[0,9,18,27,28,29,36,37,38,46,47,48,49,50,51,52,53,54,63,72],[1,10,19,27,28,29,36,37,38,45,47,48,49,50,51,52,53,55,64,73],[2,11,20,27,28,29,36,37,38,45,46,48,49,50,51,52,53,56,65,74],[3,12,21,30,31,32,39,40,41,45,46,47,49,50,51,52,53,57,66,75],[4,13,22,30,31,32,39,40,41,45,46,47,48,50,51,52,53,58,67,76],[5,14,23,30,31,32,39,40,41,45,46,47,48,49,51,52,53,59,68,77],[6,15,24,33,34,35,42,43,44,45,46,47,48,49,50,52,53,60,69,78],[7,16,25,33,34,35,42,43,44,45,46,47,48,49,50,51,53,61,70,79],[8,17,26,33,34,35,42,43,44,45,46,47,48,49,50,51,52,62,71,80],[0,9,18,27,36,45,55,56,57,58,59,60,61,62,63,64,65,72,73,74],[1,10,19,28,37,46,54,56,57,58,59,60,61,62,63,64,65,72,73,74],[2,11,20,29,38,47,54,55,57,58,59,60,61,62,63,64,65,72,73,74],[3,12,21,30,39,48,54,55,56,58,59,60,61,62,66,67,68,75,76,77],[4,13,22,31,40,49,54,55,56,57,59,60,61,62,66,67,68,75,76,77],[5,14,23,32,41,50,54,55,56,57,58,60,61,62,66,67,68,75,76,77],[6,15,24,33,42,51,54,55,56,57,58,59,61,62,69,70,71,78,79,80],[7,16,25,34,43,52,54,55,56,57,58,59,60,62,69,70,71,78,79,80],[8,17,26,35,44,53,54,55,56,57,58,59,60,61,69,70,71,78,79,80],[0,9,18,27,36,45,54,55,56,64,65,66,67,68,69,70,71,72,73,74],[1,10,19,28,37,46,54,55,56,63,65,66,67,68,69,70,71,72,73,74],[2,11,20,29,38,47,54,55,56,63,64,66,67,68,69,70,71,72,73,74],[3,12,21,30,39,48,57,58,59,63,64,65,67,68,69,70,71,75,76,77],[4,13,22,31,40,49,57,58,59,63,64,65,66,68,69,70,71,75,76,77],[5,14,23,32,41,50,57,58,59,63,64,65,66,67,69,70,71,75,76,77],[6,15,24,33,42,51,60,61,62,63,64,65,66,67,68,70,71,78,79,80],[7,16,25,34,43,52,60,61,62,63,64,65,66,67,68,69,71,78,79,80],[8,17,26,35,44,53,60,61,62,63,64,65,66,67,68,69,70,78,79,80],[0,9,18,27,36,45,54,55,56,63,64,65,73,74,75,76,77,78,79,80],[1,10,19,28,37,46,54,55,56,63,64,65,72,74,75,76,77,78,79,80],[2,11,20,29,38,47,54,55,56,63,64,65,72,73,75,76,77,78,79,80],[3,12,21,30,39,48,57,58,59,66,67,68,72,73,74,76,77,78,79,80],[4,13,22,31,40,49,57,58,59,66,67,68,72,73,74,75,77,78,79,80],[5,14,23,32,41,50,57,58,59,66,67,68,72,73,74,75,76,78,79,80],[6,15,24,33,42,51,60,61,62,69,70,71,72,73,74,75,76,77,79,80],[7,16,25,34,43,52,60,61,62,69,70,71,72,73,74,75,76,77,78,80],[8,17,26,35,44,53,60,61,62,69,70,71,72,73,74,75,76,77,78,79]]
-- matchableCellIndexes idx = let res = delete idx (nub ((rowIndexRange (cellRow idx)) ++ (columnIndexRange (cellColumn idx)) ++ (boardRanges (cellBoard idx)))) in res `seq` res

matchableCellsFor :: Int -> V.Vector Cell -> V.Vector Cell
matchableCellsFor idx = sliceCells (matchableCellIndexes idx)

showCell :: Cell -> String
showCell (Empty _ _) = "_"
showCell (Entry _ i) = show i

showBoard :: [Cell] -> String
showBoard b = foldl' (\x y -> x ++ (showCell y)) "" b

boardComplexity :: Board -> Int
boardComplexity (SolvedBoard _) = 0
boardComplexity (UnsolvedBoard b) = V.foldl' howComplexIs 0 b

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

sliceCells :: [Int] -> V.Vector Cell -> V.Vector Cell
sliceCells i b = V.map (b V.!) (V.fromList i)

row :: Int -> V.Vector Cell -> V.Vector Cell
row = sliceCells . rowIndexRange

column :: Int -> V.Vector Cell -> V.Vector Cell
column = sliceCells . columnIndexRange

subBoard :: Int -> V.Vector Cell -> V.Vector Cell
subBoard = sliceCells . boardRanges
