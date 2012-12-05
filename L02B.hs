module L02B where
-- Examples from Week 2, lecture B
-- Higher-Order Functions
-- dave 2012-11-08

import Data.List(sort,group)
import Data.Char(isSpace)
import Test.QuickCheck
import Test.QuickCheck.Function -- for testing HO functions

------------------------------------------
verse = 
  "A dying mosquito exclaimed,\nA chemist has poisoned my brain!\nThe cause of his sorrow\nWas para-dichloro-\nDiphenyl-trichloroethane\n"

-- map examples, abs, shout
-- def with list comprehensions

map' f xs    = [f x | x <- xs]
filter' p xs = [x | x <- xs, p x]

sum' []     = 0
sum' (n:ns) = n + sum' ns

and' [] = True
and' (b:bs) = b && and' bs

foldr' op b [] = b
foldr' op b (x:xs) = x `op` foldr' op b xs 
-- foldr 


-- foldr op z xs
-- foldl op z [a,b,c,d] == ((z `op` a) `op` b) ...

unlines' ss = foldr joinNl "" ss
        where joinNl s1 s2 = s1 ++ "\n" ++ s2

-- takeLine, takeWord
takeLine ""                 = ""
takeLine (c:cs) | c /= '\n' = c:takeLine cs
                | otherwise = ""
takeWord ""                      = ""
takeWord (c:cs) | not(isSpace c) = c:takeWord cs
                | otherwise      = ""

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []     = []
takeWhile' p (x:xs) 
        | p x       = x:takeWhile' p xs   
        | otherwise = []

-- takeLine'' cs = takeWhile' (\x -> x /= '\n') cs 
takeLine'' = takeWhile' (/= '\n')  

-- Another pattern: lines, commaSep

lines' [] = []
lines' css = takeWhile (/= '\n') css 
     : lines' (drop 1 (dropWhile (/= '\n') css))

nosh = "spam,eggs,chips,spam"
commSep [] = []
commSep css = takeWhile (/= ',') css 
     : commSep (drop 1 $ dropWhile (/= ',') css)

-- segments -- see slides
-------------------------------------
-- More ways to build functions: Partial application 

-- redundant brackets in the type help explain partial application:

-- meaningless function
f :: Char -> (Bool -> (String -> String)) -- redundant brackets
f c b s = c:s ++ show (not b) ++ s 

-- Eta reduction
-- f x = e x  can be rewritten as f = e
-- f y x = e x    -->  f y = e

-- use hlint <filename> to find this kind of thing

-- Aside 
-- Fixity.  How do you know when to skip brackets?
-- $ 
 
-- Example Design (Slides)
-- Function composition 

wordCount = putStr
          . unlines
          . map (\(w,n) -> w ++ ": " ++ show n)
          . map (\ws -> (head ws,length ws)) 
          . group 
          . sort 
          . words 

prop_mapmap (Fun _ f) (Fun _ g) xs = map f (map g xs) == map (f.g) xs
  where types = (f :: Int -> Int, g :: Int -> Int)


-- foldr puzzles

-- foldr (:) ys xs
-- foldr atEndOf [] xs where y `atEndof` ys = ys ++ [y]
-- foldr (\y ys -> f y : ys) [] xs
