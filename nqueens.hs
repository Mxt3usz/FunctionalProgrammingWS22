-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
module NQueens where

import qualified Data.List as List
import SimplePrelude
import Prelude ()

-- | A list of columns. The integer in each column specifies at which 0-based
-- index the queen is positioned.
type Solution = [Integer]

-------------------------------------------------------------------------------
-- Search algorithm

nqueens :: Integer -> [Solution]
nqueens = error "TODO"
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
displaySolution :: Integer -> Integer -> Solution -> String
displaySolution rows cols solution =
  unlines
    . List.transpose
    . List.genericTake cols
    $ map displayColumn solution ++ repeat emptyColumn
  where
    displayColumn n =
      List.genericTake rows $
        List.genericReplicate n 'â‹…' ++ 'â™›' : List.genericReplicate (rows - n - 1) 'â‹…'
    emptyColumn =
      List.genericReplicate rows 'â‹…'