module Sudoku.SolutionMethods
(bruteSolve)
 where

import Sudoku.Board
import Data.List
import Data.Maybe

simplifyByElimination :: Maybe Cell -> Cell -> Maybe Cell
simplifyByElimination Nothing _ = Nothing
simplifyByElimination en@(Just (Entry _ _)) _ = en
simplifyByElimination en@(Just (Empty _ _)) (Empty _ _) = en
simplifyByElimination (Just (Empty idx vals)) (Entry _ val) = let remaining = delete val vals in
                                                                 case remaining of
                                                                   [] -> Nothing
                                                                   otherwise -> Just (Empty idx remaining)

matchableCellsFor :: Int -> [Cell] -> [Cell]
matchableCellsFor idx = sliceCells (nub ((rowIndexRange (cellRow idx)) ++ (columnIndexRange (cellColumn idx)) ++ (boardRanges (cellBoard idx))))

simpleReduceCell :: [Cell] -> Cell -> Maybe Cell
simpleReduceCell b en@(Entry _ _) = Just en
simpleReduceCell b en@(Empty idx []) = Nothing
simpleReduceCell b en@(Empty idx vals) = foldl' (simplifyByElimination) (Just en) (matchableCellsFor idx b)

exclusionPass :: [Cell] -> Maybe [Cell] 
exclusionPass b = sequence $ map (\c -> simpleReduceCell b c) b

exclusionReduce :: Board -> Maybe Board
exclusionReduce sb@(SolvedBoard _) = Just sb
exclusionReduce usb@(UnsolvedBoard b) = let reduction = fmap (maybeSolve . UnsolvedBoard) (exclusionPass b) in
                                            case reduction of
                                              Nothing -> Nothing
                                              Just (SolvedBoard x) -> Just (SolvedBoard x)
                                              Just y -> if ((boardComplexity y) == (boardComplexity usb)) then reduction else exclusionReduce y
                                              

furcateSolutions :: Board -> [Board]
furcateSolutions sb@(SolvedBoard _) = [sb]
furcateSolutions (UnsolvedBoard b) = catMaybes (map (\x -> exclusionReduce (UnsolvedBoard x)) (splitOnGuess b))

bruteSolve :: Board -> [Board]
bruteSolve b = runBruteSteps [b]

runBruteSteps :: [Board] -> [Board]
runBruteSteps [] = []
runBruteSteps x | all boardComplete stepResults = stepResults
                | otherwise = runBruteSteps stepResults
                where stepResults = concatMap furcateSolutions x
