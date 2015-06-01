module Sudoku.SolutionMethods
 where

import Sudoku.Board
import Data.List

simplifyByElimination :: Cell -> Cell -> Cell
simplifyByElimination en@(Entry _ _) _ = en
simplifyByElimination en@(Empty _ _) (Empty _ _) = en
simplifyByElimination en@(Empty idx vals) (Entry _ val) = Empty idx (delete val vals)

matchableCellsFor :: Int -> [Cell] -> [Cell]
matchableCellsFor idx = sliceCells ((rowIndexRange (cellRow idx)) ++ (columnIndexRange (cellColumn idx)) ++ (boardRanges (cellBoard idx)))

simpleReduceCell :: [Cell] -> Cell -> Cell
simpleReduceCell b en@(Entry _ _) = en
simpleReduceCell b en@(Empty idx []) = en
simpleReduceCell b en@(Empty idx vals) = foldl' (simplifyByElimination) en (matchableCellsFor idx b)

lastReduce :: Cell -> Cell
lastReduce en@(Entry _ _) = en
lastReduce en@(Empty idx []) = en
lastReduce (Empty idx [x]) = Entry idx x
lastReduce (Empty idx xs) = Empty idx xs

exclusionPass :: [Cell] -> [Cell] 
exclusionPass b = map (\c -> simpleReduceCell b c) b

exclusionReduce :: Board -> Board
exclusionReduce sb@(SolvedBoard _) = sb
exclusionReduce nvb@(UnsolvedBoard False b) = nvb
exclusionReduce usb@(UnsolvedBoard True b) | (boardComplexity usb) == (boardComplexity result) = result
                                       | otherwise = exclusionReduce result
                                         where result = (maybeSolve . markValidity) (UnsolvedBoard True (exclusionPass b))

furcateSolutions :: Board -> [Board]
furcateSolutions sb@(SolvedBoard _) = [sb]
furcateSolutions (UnsolvedBoard False b) = []
furcateSolutions (UnsolvedBoard True b) = filter isBoardValid (map (\x -> exclusionReduce (UnsolvedBoard True x)) (splitOnGuess b))

bruteSolve :: Board -> [Board]
bruteSolve b = runBruteSteps [exclusionReduce b]

runBruteSteps :: [Board] -> [Board]
runBruteSteps [] = []
runBruteSteps x | all boardComplete stepResults = stepResults
                | otherwise = runBruteSteps stepResults
                where stepResults = concatMap furcateSolutions x
