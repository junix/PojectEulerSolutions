{-# LANGUAGE ParallelListComp #-}
import Data.List (nub, (\\), sortBy)
import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.Char
import Control.Lens

{-
Su Doku (Japanese meaning number place) is the name given to a popular
puzzle concept. Its origin is unclear, but credit must be attributed to
Leonhard Euler who invented a similar, and much more difficult, puzzle
idea called Latin Squares. The objective of Su Doku puzzles, however, is
to replace the blanks (or zeros) in a 9 by 9 grid in such that each row,
column, and 3 by 3 box contains each of the digits 1 to 9. Below is an
example of a typical starting puzzle grid and its solution grid.

       +-----------------------+         +-----------------------+
       | 0 0 3 | 0 2 0 | 6 0 0 |         | 4 8 3 | 9 2 1 | 6 5 7 |
       | 9 0 0 | 3 0 5 | 0 0 1 |         | 9 6 7 | 3 4 5 | 8 2 1 |
       | 0 0 1 | 8 0 6 | 4 0 0 |         | 2 5 1 | 8 7 6 | 4 9 3 |
       |-------+-------+-------|         |-------+-------+-------|
       | 0 0 8 | 1 0 2 | 9 0 0 |         | 5 4 8 | 1 3 2 | 9 7 6 |
       | 7 0 0 | 0 0 0 | 0 0 8 |         | 7 2 9 | 5 6 4 | 1 3 8 |
       | 0 0 6 | 7 0 8 | 2 0 0 |         | 1 3 6 | 7 9 8 | 2 4 5 |
       |-------+-------+-------|         |-------+-------+-------|
       | 0 0 2 | 6 0 9 | 5 0 0 |         | 3 7 2 | 6 8 9 | 5 1 4 |
       | 8 0 0 | 2 0 3 | 0 0 9 |         | 8 1 4 | 2 5 3 | 7 6 9 |
       | 0 0 5 | 0 1 0 | 3 0 0 |         | 6 9 5 | 4 1 7 | 3 8 2 |
       +-----------------------+         +-----------------------+

A well constructed Su Doku puzzle has a unique solution and can be solved
by logic, although it may be necessary to employ "guess and test" methods
in order to eliminate options (there is much contested opinion over this).
The complexity of the search determines the difficulty of the puzzle; the
example above is considered easy because it can be solved by straight
forward direct deduction.

The 6K text file sudoku.txt contains fifty different Su Doku puzzles ranging
in difficulty, but all with unique solutions (the first puzzle in the file is
the example above).

By solving all fifty puzzles find the sum of the 3-digit numbers found in
the top left corner of each solution grid; for example, 483 is the 3-digit
number found in the top left corner of the solution grid above.
-}

buildSudokuList xs = map (map (map ordx)) . map tail . chunksOf 10 $ xs
leftTop3 mat = sum [d*(10^e) | d <- take 3 . head $ mat | e <- [2,1..0]]
ordx = toInteger . (subtract.ord $ '0') . ord
readCell mat (x,y) = mat !! y !! x
writeCell mat (x,y) v = mat & ix y .~ ((mat !! y) & ix x .~ v)
probCells mat poss = ([1..9] \\) . filter (>0) . map (readCell mat) $ poss
calcCells (x,y) = smallGrid ++ rowCells ++ colCells
    where mov n = n - (n `rem` 3)
          smallGrid = [(a+dx, b+dy) | let dx = mov x, let dy = mov y, a <- [0..2] , b <- [0..2] ]
          rowCells  = [(x,v) | v <- [0..8]]
          colCells  = [(v,y) | v <- [0..8]]
collectEmptyPos mat = sortBy (compare `on` (length.snd)) $
    [ (pos, cells) | pos <- allPos
    , readCell mat pos == 0
    , let cells = probCells mat . calcCells $ pos
    ]
    where allPos = [ (x, y) | x <- [0..8] , y <- [0..8] ]
solve mat = go . collectEmptyPos $ mat
    where go [] = [mat]
          go ((pos,[]):_) = []
          go ((pos,vs):_) = take 1 . concatMap (solve . writeCell mat pos) $ vs
euler c = sum . map leftTop3 . concatMap solve . buildSudokuList . lines $ c
main = readFile "./p096_sudoku.txt" >>= print.euler
