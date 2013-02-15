-- file QC-basics.hs

{-# LANGUAGE TemplateHaskell #-}

{- 
Real World Haskell 
Chapter 11: Testing and quality assurance

QuickCheck Basics
http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
-}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Control.Monad(liftM,liftM2)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs
          
prop_idempotent :: [Integer] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum xs = not (null xs) ==> 
                  head (qsort xs) == minimum xs
                  
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)
         
prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)
          
prop_maximum xs = not (null xs) ==>
                  last (qsort xs) == maximum xs
                  
prop_append xs ys = not (null xs) ==>
                    not (null ys) ==>
                    head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
                    
prop_sort_model xs = sort xs == qsort xs


data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)
                  
{-instance Arbitrary Char where
  arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")
-}
  
instance Arbitrary Doc where
  arbitrary =
    oneof [ return Empty
          , liftM Char arbitrary
          , liftM Text arbitrary
          , return Line
          , liftM2 Concat arbitrary arbitrary
          , liftM2 Union arbitrary arbitrary
          ]
    
empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

prop_empty_id x = 
    empty <> x == x
  &&
    x <> empty == x
    
prop_char c   = char c   == Char c

prop_text s   = text s   == if null s then Empty else Text s

prop_line     = line     == Line

prop_double d = double d == text (show d)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

prop_hcat xs = hcat xs == glue xs
    where
      glue []     = empty
      glue (d:ds) = d <> glue ds
      
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

prop_punctuate s xs = punctuate s xs == combine (intersperse s xs)
    where
      combine [] = []
      combine [x] = [x]
      combine (x:Empty:ys) = x : combine ys
      combine (Empty:y:ys) = y : combine ys
      combine (x:y:ys)     = x `Concat` y : combine ys
      
      
runTests = $(quickCheckAll)

main = do runTests
          
-- (% is prompt)
-- % ghc -fhpc QC-basics.hs --make
-- % ./QC-basics
-- % hpc markup QC-basics.tix --exclude=QuickCheck
-- % browser hpc_index.html
