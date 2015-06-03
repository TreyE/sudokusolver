module SudokuSolver (main) where

import Sudoku.SolutionMethods
import SudokuParser

-- input = "_____1__5_657______2_59_3___36____9_____4_____9____68___3_58_7______294_7__3_____"
--input = "____________7______2_59_3___36____9_____4_____9____68___3_58_7______294_7__3_____"
input = "8__________36______7__9_2___5___7_______457_____1___3___1____68__85___1__9____4__"

main :: IO ()
main = case parse input of
        Nothing -> putStrLn "Invalid Board"
        Just x -> putStrLn (show (runSimplification x))

runSimplification x = bruteSolve x
