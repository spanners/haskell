module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck.Instances.List

-------------------------------------------------------------------------

-- A.

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving (Show,Eq)

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $
                 replicate 9 (replicate 9 Nothing)

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku board) = length board == 9 
                          && and [isSudokuRow oneNineNothing x 
                                 | x <- board]

isSudokuRow :: [Maybe Int] -> [Maybe Int] -> Bool
isSudokuRow nums row = length row == 9 
                       && and [x `elem` nums 
                              | x <- row]

oneToNine :: [Maybe Int]
oneToNine = map Just [1..9]

oneNineNothing = Nothing:oneToNine

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = isSudoku sud 
               && and [isSudokuRow oneToNine x 
                      | x <- (rows sud)]

-------------------------------------------------------------------------

-- B.

{-
instance Show Sudoku where
  show = show Sudoku
    where    
      showSudoku (Sudoku board) = unlines $ map (map showCell) board
        where
          showCell (Just n) = (head . show) n
          showCell (Nothing) = '.'
-}
-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = 
  do s <- readFile fp
     return (Sudoku (map readSud (lines s)))
       where
         readSud [] = []
         readSud (x:xs) | x == '.'            = Nothing : (readSud xs)
                        | x `elem` ['1'..'9'] = Just (digitToInt x) : (readSud xs)
-------------------------------------------------------------------------

-- C.

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


newtype Filled = Filled Sudoku


fullCell :: Gen (Maybe Int)
fullCell = do x <- elements [1..9]
              return $ Just x

instance Arbitrary Filled where
    arbitrary =
        do rs <- sequence [ sequence [ fullCell | j <- [1..9] ] | i <- [1..9] ] 
           return (Filled (Sudoku rs))

instance Show Filled where
    show (Filled s) = show s

-----------------------------------------------------------------------------

-- D.

type Block = [Maybe Int]

type IndexedBlock = [(Pos, Maybe Int)]

isOkayBlock :: Block -> Bool
isOkayBlock b = b' == nub b'
  where b' = filter (/=Nothing) b

blocks :: Sudoku -> [Block]
blocks (Sudoku sud) = sud ++ 
                      transpose sud ++ 
                      threeByThrees sud

threeByThrees s = concat [threeRows s row | row <- [3,6,9]] 
                  
threeRows s row = [concat [drop (x-3) (take x y) 
                            | y <- (drop (row-3) 
                                    (take row s))] 
                    | x <- [3,6,9]]

isOkay :: Sudoku -> Bool
isOkay s = and $ map isOkayBlock (blocks s)

prop_blocks :: Sudoku -> Bool
prop_blocks s =  length [True | x <- blocks s, length x == 9] == 27


-- NEW SHIT

pairs xs ys = map (flip map xs . (,)) ys

indexedSudoku :: Sudoku -> [IndexedBlock]
indexedSudoku = zipWith zip (pairs [0..] [0..]) . rows


indexedBlocks :: Sudoku -> [IndexedBlock]
indexedBlocks sud = sud' ++
                    transpose sud' ++
                    threeByThrees sud'
  where sud' = indexedSudoku sud

-------------------------------------------------------------------------------

-- E.

type Pos = (Int, Int)

blank :: Sudoku -> Pos
blank = fst . head . filterNothings . blockWithLeastBlanksIndexed . indexedBlocks

prop_injectBlank :: Filled -> Int -> Int  -> Bool
prop_injectBlank (Filled s) x y = blank (update s (x', y') Nothing) == (x', y')
    where x' = x `mod` 9
          y' = y `mod` 9

prop_findAllBlanks :: Filled -> [Pos] -> Property
prop_findAllBlanks (Filled s) ps = ps /= [] ==> 
                                     sort (findAllBlanks (foo s ps')) ==  sort ps' 
                                         where 
					   ps' = select 10 ps

data PosList = PosList [Pos]

instance Arbitrary PosList where
  arbitrary = 
    do
      x <- arbitrary
      y <- arbitrary
      return infiniteList $ PosList $ (x `mod` 9, y `mod` 9)

select n = (nub . (map modPos) . nub . take n)
modPos (x, y) = (x `mod` 9, y `mod` 9)
foo s ps' = updatePoss s ps' Nothing


findAllBlanks :: Sudoku -> [Pos]

findAllBlanks s | isSolved s = []
                | otherwise  = poop:(findAllBlanks (update s poop (Just 1)))
    where poop = blank s

updatePoss s []     item = s
updatePoss s (p:ps) item = updatePoss (update s p item) ps item

{-
11:15 <@Iceland_jack> you can also take a filled grid and inject n blanks at random locations
11:15 <+SwashBuckla> that's one test
11:16 <+SwashBuckla> ah but I don't know what blank should get found by the blank finder then
11:16 <@Iceland_jack> then make sure your blank finder finds them all (possbily filling the blanks after each successful find)
-}

-- [(Pos, Maybe Int)]
blockWithLeastBlanksIndexed :: [IndexedBlock] -> IndexedBlock
blockWithLeastBlanksIndexed bs = snd $ minimum $ zip (map numBlankSpotsIndexed $ bs') bs'
  where bs' = filter ((/=0) . numBlankSpotsIndexed) bs

filterNothings :: IndexedBlock -> IndexedBlock
filterNothings []                   = []
filterNothings (((a,b),Nothing):rest) = ((a,b),Nothing):(filterNothings rest)
filterNothings (_:rest)               = filterNothings rest

numBlankSpotsIndexed :: IndexedBlock -> Int
numBlankSpotsIndexed block = sum [count ((n,m), Nothing) block | n <- [0..8],  m <- [0..8]]

numBlankSpots :: Block -> Int
numBlankSpots = count Nothing

count :: (Eq a) => a -> [a] -> Int
count x ys = length (filter (== x) ys)
  

(!!=) :: [a] -> (Int, a) -> [a]
(!!=)  [] _ = []
(!!=) (x:xs) (n,c)
        | n    == 0 = c:xs
        | otherwise = x : xs !!= (n-1,c)

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku board) (n,m) i = 
  Sudoku ((take n board) ++ 
          [(board !! n) !!= (m, i)] ++ 
          (drop (n+1) board))

prop_isBlank :: Sudoku -> Bool
prop_isBlank s = (update s (blank s) Nothing) == s
 
prop_replaceThing :: Eq a => [a] -> (Int, a) -> Property
prop_replaceThing ls (n, c) = ls /= [] ==>
                              c == (ls !!= (n,c)) !! (n `mod` (length ls))

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update s (a,b) i = (rows (update s (a', b') i)) !! a' !! b' == i
  where a' = a `mod` 9
        b' = b `mod` 9
        
---------------------------------------------------------------------------------------

-- F.

solve :: Sudoku -> Maybe Sudoku
solve s | not (isOkay s) = Nothing
solve s | isSolved s = Just s
solve s = case filter (/= Nothing) [solve (update s (blank s) (Just i))
          | i <- [1..9]] of
          [] -> Nothing
          (x:_) -> x
          
readAndSolve :: FilePath -> IO Sudoku
readAndSolve fp = 
  do 
    sud <- readSudoku fp
    return (fromJust (solve sud))
 
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 | isSolved s1 = s1 == fromJust(solve s2)
                   | otherwise = False
 
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s =
    s' /= Nothing && isSudoku s ==> isSolutionOf (fromJust s') s
    where s' = solve s

--------------------------------------------------------------------------------------

example :: Sudoku
example = Sudoku
          [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
          , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
          , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
          , [Nothing,Nothing,Nothing,Just 9,Just 1, Just 3, Nothing,Just 2, Just 8]
          , [Just 4, Nothing,Nothing,Just 5, Just 4,Just 2, Nothing,Nothing,Just 9]
          , [Just 2, Just 7, Nothing,Just 4, Just 6, Just 9,Nothing,Nothing,Nothing]
          , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
          , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
          , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
          ]
