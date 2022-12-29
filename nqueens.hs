-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
module NQueens where

import qualified Data.List as List
import SimplePrelude
import Prelude ()

-- | A list of columns. The integer in each column specifies at which 0-based
-- index the queen is positioned.
--type Solution = [Integer]

-------------------------------------------------------------------------------
-- Search algorithm

--nqueens :: Integer -> [Solution]


-------------------------------------------------------------------------------
-- Solving & printing the solution

-- | Prints the first solution.
solve :: Integer -> IO ()
solve n = case nqueens n of
  [] -> putStrLn "No solution found."
  solution : _ -> putStr $ displaySolution n n solution

-- | Prints all solutions.
solveAll :: Integer -> IO ()
solveAll n = case nqueens n of
  [] -> putStrLn "No solution found."
  solutions -> mapM_ (putStrLn . displaySolution n n) solutions

-------------------------------------------------------------------------------
-- Visualizing

-- | Prettyprints a solution as a grid with the specified size.
