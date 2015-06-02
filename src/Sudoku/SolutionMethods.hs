{-# LANGUAGE TemplateHaskell #-}
module Sudoku.SolutionMethods
(bruteSolve)
 where

import Sudoku.Board
import Data.List
import Data.Maybe
import Control.Monad

simplifyByElimination :: Cell -> Cell -> Maybe Cell
simplifyByElimination en@(Entry _ _) _ = return en
simplifyByElimination en@(Empty _ _) (Empty _ _) = return en
simplifyByElimination (Empty idx vals) (Entry _ val)  = let remaining = delete val vals in
                                                            case remaining of
                                                              [] -> Nothing
                                                              otherwise -> Just (Empty idx remaining)

matchableCellIndexes :: Int -> [Int]
matchableCellIndexes idx = (nub ((rowIndexRange (cellRow idx)) ++ (columnIndexRange (cellColumn idx)) ++ (boardRanges (cellBoard idx))))

matchableCellsFor :: Int -> [Cell] -> [Cell]
matchableCellsFor idx = sliceCells (matchableCellIndexes idx)

simpleReduceCell :: [Cell] -> Cell -> Maybe Cell
simpleReduceCell b en@(Entry _ _) = return en
simpleReduceCell b en@(Empty idx []) = Nothing
simpleReduceCell b en@(Empty idx vals) = foldM (simplifyByElimination) en (matchableCellsFor idx b)

exclusionPass :: [Cell] -> Maybe [Cell] 
exclusionPass b = mapM (\c -> simpleReduceCell b c) b

exclusionReduce :: Board -> Maybe Board
exclusionReduce sb@(SolvedBoard _) = return sb
exclusionReduce usb@(UnsolvedBoard b) = let reduction = fmap (maybeSolve . UnsolvedBoard) (exclusionPass b) in
                                            case reduction of
                                              Nothing -> Nothing
                                              Just (SolvedBoard x) -> return (SolvedBoard x)
                                              Just y -> if ((boardComplexity y) == (boardComplexity usb)) then reduction else exclusionReduce y
                                              

furcateSolutions :: Board -> [Board]
furcateSolutions sb@(SolvedBoard _) = [sb]
furcateSolutions (UnsolvedBoard b) = catMaybes (map (\x -> exclusionReduce (UnsolvedBoard x)) (splitOnGuess b (guessSplit b)))

guessSplit :: [Cell] -> Int
guessSplit = cellPos . minimum

bruteSolve :: Board -> [Board]
bruteSolve b = runBruteSteps [b]

runBruteSteps :: [Board] -> [Board]
runBruteSteps [] = []
runBruteSteps x | all boardComplete stepResults = stepResults
                | otherwise = runBruteSteps stepResults
                where stepResults = concatMap furcateSolutions x
