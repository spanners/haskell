import Data.List
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = a `elem` (permutations b)

prop_reverse :: Eq a => [a] -> Bool
prop_reverse a = a `isPermutation` (reverse a)

duplicates :: Eq a => [a] -> Bool
duplicates = undefined
