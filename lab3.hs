import Data.List
import Test.QuickCheck

-- Exercises for Week 3: Lists and List Comprehensions
-- You may need the following useful standard functions: {or, and, nub}

-- 1. Permutations

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = a `elem` (permutations b)

prop_reversePermutation :: Eq a => [a] -> Bool
prop_reversePermutation a = a `isPermutation` (reverse a)

-- 2. Avoiding Duplicates

duplicates :: Eq a => [a] -> Bool
duplicates a = a /= nub a

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates a = nub a

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

-- 3. Pascal's Triangle

fac = product . enumFromTo 1
binCoef n k = (fac n) `div` ((fac k) * (fac $ n - k))
pascal n = map (binCoef $ n - 1) [0..n-1]

-- 4. Erastosthenes' sieve

crossOut :: Int -> [Int] -> [Int]
crossOut n xs = filter (\a -> a `mod` n /= 0) xs

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (crossOut x xs)

-- 5. Number Games

isPrime :: Int -> Bool
isPrime n = n `elem` sieve [2..100]

isSumOfTwoPrimes n = undefined

-- 6. Occurrences in Lists

occursIn :: Eq a => a -> [a] -> Bool
occursIn a xs = a `elem` xs


allOccursIn :: Eq a => [a] -> [a] -> Bool
allOccursIn xs ys = xs `isPermutation` ys

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = and [x == y | (x,y) <- xs `zip` ys]

numOccurences :: Eq a => a -> [a] -> Int
numOccurences x []              = 0
numOccurences x (a:as) | x /= a = numOccurences x as
numOccurences x (a:as) | x == a = 1 + numOccurences x as

remove :: (a -> Bool) -> [a] -> [a]
remove p = filter (not . p)

bag :: Eq a => [a] -> [(a, Int)]
bag [] = []
bag (x:xs) = (x, numOccurences x (x:xs)) : bag (remove (==x) xs)

-- 7. Elements and Positions

itemPosition :: Eq a => a -> [a] -> [Int]
itemPosition x xs = [index | (index,elem) <- [0..] `zip` xs, elem == x] 

positions :: Eq a => [a] -> [(a, [Int])]
positions xs = nub (xs `zip` [(itemPosition x xs) | x <- xs])

-- 8. More List Comprehensions

-- this seems to do a cartesian product of xs and ys
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x <-xs, y<-ys]

pythagorean_triad :: [(Int, Int, Int)]
pythagorean_triad = [(a,b,c) | a <- [0..100], b <- [0..100], c <- [0..100], a^2 + b^2 == c^2]