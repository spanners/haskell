module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

-------------------------------------------------------------------------

-- A.

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving Eq

isValue :: Maybe Int -> Bool
isValue Nothing = True
isValue (Just x) = x >= 1 && x <= 9

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $
                 replicate 9 (replicate 9 Nothing)

isSudoku :: Sudoku -> Bool
isSudoku s =  and[length r == 9 && and[isValue x | x <- r] | r <- rows s]
              && length (rows s) == 9


isSolved :: Sudoku -> Bool
isSolved (Sudoku s) = notElem Nothing (concat s)

-------------------------------------------------------------------------

-- B.


instance Show Sudoku where
  show (Sudoku s) = show s --Sudoku
    where    
      showSudoku (Sudoku board) = unlines $ map (map showCell) board
        where
          showCell (Just n) = (head . show) n
          showCell (Nothing) = '.'

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

cell :: Gen (Maybe Int)
cell = frequency [(9,blank), (1,num)]
  where
    blank = return Nothing
    num = do
      x <- elements [1..9]
      return $ Just x

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
        do rows <- sequence [ sequence [ fullCell | j <- [1..9] ] | i <- [1..9] ] 
           return (Filled (Sudoku rows))

instance Show Filled where
    show (Filled s) = show s
    
type Pos = (Int, Int)

newtype PosList = PosList [Pos]    

instance Arbitrary PosList where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    PosList n <- arbitrary
    return $ PosList ((x `mod` 9, y `mod` 9) : n)

instance Show PosList where
  show (PosList p) = show p

-----------------------------------------------------------------------------

-- D.

type IndexedBlock = [(Pos, Maybe Int)]

blocks :: Sudoku -> [IndexedBlock]
blocks sud = sud' ++
             transpose sud' ++
             threeByThrees sud'
  where sud' = indexedSudoku sud
        
indexedSudoku :: Sudoku -> [IndexedBlock]
indexedSudoku = zipWith zip (pairs [0..] [0..]) . rows

pairs xs ys = map (flip map xs . (,)) ys

isOkayBlock :: IndexedBlock -> Bool
isOkayBlock b = b' == nub b'
  where b' = filter ((/=Nothing) . snd) b

threeByThrees s = concat [threeRows s row | row <- [3,6,9]] 
                  
threeRows s row = [concat [drop (x-3) (take x y) 
                            | y <- (drop (row-3) 
                                    (take row s))] 
                    | x <- [3,6,9]]

isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock b | b <- blocks s]

prop_blocks :: Sudoku -> Bool
prop_blocks s =  length [True | x <- blocks s, length x == 9] == 27

-------------------------------------------------------------------------------

-- E.

blank :: Sudoku -> Pos
blank = fst 
        . head 
        . filter ((==Nothing) . snd) 
        . leastBlanks 
        . blocks

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
 
prop_injectBlank :: Filled -> Int -> Int  -> Bool
prop_injectBlank (Filled s) x y = blank (update s (x', y') Nothing) == (x', y')
    where x' = x `mod` 9
          y' = y `mod` 9

prop_findAllBlanks :: Filled -> PosList -> Bool
prop_findAllBlanks (Filled s) (PosList ps) = sort (findAllBlanks (injectBlanks s ps')) == sort ps' 
  where 
    ps' = (nub . take 40) ps

findAllBlanks :: Sudoku -> [Pos]
findAllBlanks s | isSolved s = []
                | otherwise  = b:(findAllBlanks (update s b (Just 0)))
    where b = blank s

injectBlanks s []     = s
injectBlanks s (p:ps) = injectBlanks (update s p Nothing) ps

leastBlanks :: [IndexedBlock] -> IndexedBlock
leastBlanks bs = (snd . minimum) $ zip (map numBlanks bs') bs'
  where bs' = filter ((/=0) . numBlanks) bs

numBlanks :: IndexedBlock -> Int
numBlanks block = length $ filter ((==Nothing) . snd) block

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
solve s = case [solve (update s (blank s) (Just i)) | i <- [1..9]] of
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
