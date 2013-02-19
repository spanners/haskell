{-# LANGUAGE TemplateHaskell #-} -- for running quickCheckAll

module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Char
import Data.Maybe
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving Eq

isValue :: Maybe Int -> Bool
isValue Nothing   = True
isValue (Just x) = x >= 1 && x <= 9 

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

isSudoku :: Sudoku -> Bool
isSudoku s =  and [ length r == 9 && 
                    and [ isValue x | x <- r ] 
                  | r <- rows s] 
              && length (rows s) == 9 

isSolved :: Sudoku -> Bool
isSolved (Sudoku board) =  Nothing `notElem` concat board

-------------------------------------------------------------------------

instance Show Sudoku where
  show = showSudoku
    where 
      showSudoku (Sudoku board) = unlines $ map (map showCell) board
        where
          showCell (Just n) = (head . show) n
          showCell Nothing  = '.'

readSudoku :: FilePath -> IO Sudoku
readSudoku fp =
  do s <- readFile fp
     return $ Sudoku $ map readSud (lines s)
    where
      readSud [] = []
      readSud (x:xs)
        | x == '.'            = Nothing : readSud xs
        | x `elem` ['1'..'9'] = Just (digitToInt x) 
                                        : readSud xs

-------------------------------------------------------------------------

cell :: Gen (Maybe Int)
cell = frequency [(9,return Nothing)
                 ,(1 , do r <- choose (1,9) 
                          return (Just r))]

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)


prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

type Block = [Maybe Int]

isBlockOkay :: Block -> Bool
isBlockOkay b = length ls == length (nub ls)
    where ls = [a | a <- b, isJust a]

blocks :: Sudoku -> [Block]
blocks (Sudoku board) = board ++ transpose board ++ threeByThrees board

isOkay :: Sudoku -> Bool
isOkay s = and [isBlockOkay b | b <- blocks s]

prop_blocks :: Sudoku -> Bool
prop_blocks s =  length [True | x <- blocks s, length x == 9] == 27

-------------------------------------------------------------------------

-- New Stuff

type IndexedBlock = [(Pos, Maybe Int)]

blank :: Sudoku -> Pos
blank = fst 
         . head
         . filter ((==Nothing) . snd)
         . leastBlanks
         . blocks'
         
leastBlanks :: [IndexedBlock] -> IndexedBlock
leastBlanks bs = (snd . minimum) $ zip (map numBlanks bs') bs'
  where bs' = filter ((/=0) . numBlanks) bs

numBlanks :: IndexedBlock -> Int
numBlanks block = length $ filter ((==Nothing) . snd) block

blocks' :: Sudoku -> [IndexedBlock]
blocks' sud = sud' ++ transpose sud' ++ threeByThrees sud'
  where sud' = indexedBoard sud
        
indexedBoard :: Sudoku -> [IndexedBlock]
indexedBoard = zipWith zip (map (map Pos) (pairs [0..] [0..])) . rows

threeByThrees s = concat [threeRows s row | row <- [3,6,9]] 
                  
threeRows s row = [concat [drop (x-3) (take x y) 
                          | y <- drop (row-3) (take row s)] 
                  | x <- [3,6,9]]

pairs xs = map (flip map xs . (,))
          
-------------------------------------------------------------------------

-- quickCheck for New Stuff

newtype Filled = Filled Sudoku
newtype PosList = PosList [Pos] 

fullCell :: Gen (Maybe Int)
fullCell = do x <- elements [1..9]
              return $ Just x

-- Generate a filled sudoku board
instance Arbitrary Filled where
    arbitrary =
        do rows <- sequence [ sequence [ fullCell 
                                       | j <- [1..9] ] 
                            | i <- [1..9] ] 
           return $ Filled $ Sudoku rows

instance Show Filled where
    show (Filled s) = show s
    
-- Generate a position
instance Arbitrary Pos where
  arbitrary = do
    (x,y) <- arbitrary
    return $ Pos (x `mod` 9, y `mod` 9)

-- Generate an infinite list of positions
instance Arbitrary PosList where
  arbitrary = do
    p <- arbitrary
    n <- arbitrary
    return $ PosList (p : n)
    
instance Show PosList where
  show (PosList p) = show p

-- Inject 1 blank into a 
prop_injectBlank :: Filled -> Pos -> Bool
prop_injectBlank (Filled s) p = blank (update s p Nothing) == p

prop_findAllBlanks :: Filled -> PosList -> Bool
prop_findAllBlanks (Filled s) (PosList ps) = 
  sort (findAllBlanks (injectBlanks s ps')) == sort ps' 
    where 
      ps' = (nub . take 40) ps

findAllBlanks :: Sudoku -> [Pos]
findAllBlanks s | isSolved s = []
                | otherwise  = b : findAllBlanks (update s b (Just 0))
  where b = blank s

injectBlanks :: Sudoku -> [Pos] -> Sudoku
injectBlanks s = foldl (\ s p -> update s p Nothing) s

-------------------------------------------------------------------------

data Pos = Pos (Int, Int)
  deriving (Ord, Eq)
           
instance Show Pos where
  show (Pos p) = show p

(!!=) :: [a] -> (Int, a) -> [a]
(!!=)  [] _ = []
(!!=) (x:xs) (n,c)
        | n    == 0 = c:xs
        | otherwise = x : xs !!= (n-1,c)

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku s) (Pos (x,y)) i = Sudoku $ 
                            take x s 
                            ++ [update' (s !! x)] 
                            ++ drop (x+1) s
  where update' xs = xs !!= (y, i)

prop_isBlank :: Sudoku -> Bool
prop_isBlank s = update s (blank s) Nothing == s

prop_replaceThing :: Eq a => [a] -> (Int, a) -> Property
prop_replaceThing ls (n, c) = ls /= [] ==> 
                              c == (ls !!= (n',c)) !! n'
  where n' = n `mod` length ls
                              
prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update s (Pos (a,b)) i =  (rows (update s (Pos (a,b)) i) !! a) !! b == i
                         
-------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve s | not (isOkay s) = Nothing
        | isSolved s     = Just s
        | otherwise      = case filter (/= Nothing) 
                                [solve 
                                 (update s (blank s) (Just i)) 
                                | i <- [1..9]] 
                           of
                             [] -> Nothing
                             (x:_) -> x
                                   
readAndSolve :: FilePath -> IO Sudoku
readAndSolve fp = 
  do 
    sud <- readSudoku fp
    return $ fromJust $ solve sud

isSolutionOf :: Sudoku -> Sudoku -> Bool 
isSolutionOf s1 s2 | isSolved s1 = s1 == fromJust(solve s2)
                   | otherwise = False

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isJust s' && isSudoku s ==> 
                    isSolutionOf (fromJust s') s
  where s' = solve s

-------------------------------------------------------------------------
          
runTests = $(quickCheckAll)

-------------------------------------------------------------------------

example :: Sudoku
example =
  Sudoku
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

-------------------------------------------------------------------------

main = runTests
