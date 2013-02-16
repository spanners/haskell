module Sudoku where

import Test.QuickCheck
import Data.List
-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving Eq

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $
                 replicate 9 (replicate 9 Nothing)

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = length (rows sud) == 9 && and [isSudokuRow oneNineNothing x | x <- (rows sud)]

isSudokuRow :: [Maybe Int] -> [Maybe Int] -> Bool
isSudokuRow nums row = length row == 9 && and [x `elem` nums | x <- row]

oneToNine :: [Maybe Int]
oneToNine = map Just [1..9]

oneNineNothing = Nothing:oneToNine

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = isSudoku sud && and [isSudokuRow oneToNine x | x <- (rows sud)]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen

instance Show Sudoku where
  show = showSudoku
    where    
      showSudoku (Sudoku board) = unlines $ map (map showCell) board
        where
          showCell (Just n) = (head . show) n
          showCell (Nothing) = '.'

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9,blank), (1,num)]
  where
    blank = return Nothing
    num = do
      x <- elements [1..9]
      return $ Just x

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-----------------------------------------------------------------------------

-- D.

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock b = b' == nub b'
  where b' = filter (/=Nothing) b

{-
blocks :: Sudoku -> [Block]
blocks (Sudoku board) = board ++ 
                        transpose board ++
                        threeByThrees board
-}                      
threeByThrees board = [take 3 $ (map $ take 3) (drop x $ board) | x <- [0,3,6]]

example :: Sudoku
example = Sudoku
          [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
          , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
          , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
          , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
          , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
          , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
          , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
          , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
          , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
          ]