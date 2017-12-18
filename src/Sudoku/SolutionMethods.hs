{-# LANGUAGE TemplateHaskell #-}
module Sudoku.SolutionMethods
(bruteSolve)
 where

import Sudoku.Board
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Vector as V

simplifyByElimination :: Cell -> Cell -> Maybe Cell
simplifyByElimination en@(Entry _ _) _ = return en
simplifyByElimination en@(Empty _ _) (Empty _ _) = return en
simplifyByElimination (Empty idx vals) (Entry _ val)  = let remaining = delete val vals in
                                                            case remaining of
                                                              [] -> Nothing
                                                              otherwise -> Just (Empty idx remaining)

simpleReduceCell :: V.Vector Cell -> Cell -> Maybe Cell
simpleReduceCell b en@(Entry _ _) = return en
simpleReduceCell b en@(Empty idx []) = Nothing
simpleReduceCell b en@(Empty idx vals) = V.foldM (simplifyByElimination) en (matchableCellsFor idx b)

shortMapMaybe :: (a -> Maybe b) -> V.Vector a -> Maybe (V.Vector b)
shortMapMaybe f a = V.foldM (\x y -> fmap (\g -> x `seq` (V.snoc x g)) (f y)) V.empty a

exclusionPass :: V.Vector Cell -> Maybe (V.Vector Cell)
exclusionPass b = shortMapMaybe (\c -> simpleReduceCell b c) b

exclusionReduce :: Board -> Maybe Board
exclusionReduce sb@(SolvedBoard _) = return sb
exclusionReduce usb@(UnsolvedBoard b) = (fmap (maybeSolve . UnsolvedBoard) (exclusionPass b)) >>= (\reduction ->
                                            case reduction of
                                              (SolvedBoard x) -> return (SolvedBoard x)
                                              otherwise -> if ((boardComplexity reduction) == (boardComplexity usb)) then return reduction else exclusionReduce reduction)
                                              


furcateSolutions :: Board -> [Board]
furcateSolutions sb@(SolvedBoard _) = [sb]
furcateSolutions (UnsolvedBoard b) = (mapMaybe (\x -> exclusionReduce (UnsolvedBoard (V.fromList x))) (splitOnGuess b (guessSplit b)))

guessSplit :: V.Vector Cell -> Int
guessSplit = cellPos . V.minimum

bruteSolve :: Board -> [Board]
bruteSolve b = runBruteSteps [b]

runBruteSteps :: [Board] -> [Board]
runBruteSteps [] = []
runBruteSteps x | all boardComplete stepResults = (filter validBoard (stepResults))
                | otherwise = runBruteSteps (filter validBoard stepResults)
                where stepResults = concatMap furcateSolutions x
