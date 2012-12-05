module Lecture02A where 
-- Functions over Lists; Classes and Polymorphism; 
-- more testing
-- David Sands 2011-10-31

import Data.List(sort)
import Test.QuickCheck

-- hide some Prelude functions so we 
-- can redefine them
import Prelude hiding ((++),reverse,drop,take) 

--import qualified Prelude as P((++),reverse,drop,take) 
-- use the original versions a P.++, P.reverse etc
{-
Plan:
  Lists; recursive definitions, polymorphism, 
  classes, quickCheck
  What are lists? 
-}

-- example list functions from Prelude
-- append
(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys =  x:(xs ++ ys)

-- reverse
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
-- (inefficient version)

rev xs = revinto [] xs
  where revinto a [] = a
        revinto a (y:ys) = revinto (y:a) ys

-- take, drop
drop,take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x:take (n-1) xs

drop n xs | n <= 0 = xs
drop _ []          = []
drop n (x:xs)      = drop (n-1) xs

-- simple property of take? (==>) 
-- we can restrict paramaters of properties with (==>)
-- or observe the generated test cases with `collect` and `classify` among others
prop_take n xs = n >= 0 ==> 
                 collect (n > length xs) $
                 length (take n xs) <= n 

prop_takedrop n xs =  classify (n <= 0 || n > length xs) "extreme" $
                      take n xs ++ drop n xs == xs
  where types = xs :: [Bool]
-- we see around 96% extreme test cases, which is bad
-- let's get better control of our data test generation using the quickcheck library for building random data generators

-- connecting take and drop?
                
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
  where smaller = [y | y <- xs, y < x]
        bigger  = [z | z <- xs, z >= x]

prop_qsort xs = sort xs == qsort xs 
  where types = xs :: [Bool]

isort [] = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
insert x (y:ys)          = y:insert x ys

prop_insert x xs = insert x (sort xs) == sort (x:xs)
  -- too similar to definition


-- Time permitting: 
-- Higher-order functions
-- filter map and foldr
