module L03B where 
--------------------------
-- Topics on Monads and
-- Test Data Generation
-- Lecture 3B, 2011
-- dave@chalmers.se 
--------------------------
import Test.QuickCheck 
import Data.List(nub,insert,sort)
import Control.Monad(liftM2) -- fmap

--------------------------

doTwice io = do 
     a <- io
     b <- io
     return (a,b)

dont io = return ()

-- QuickCheck instructions
-- QC can perform random testing with values of any type which is in class Arbitrary
-- For any type `a` in Arbitrary there is a random value generator, `Gen a`

-- IO verses Gen
-- IO A gives instructions to build a value of type A by interacting with the operating system
-- Gen A gives instructions to create a random value of type A
-- IO A is run by the ghc runtime system
-- Gen A is run by other functions in the QuickCheck library

-- sample, sample'
-- `sample` allows you to inspect generators
-- `sample'` formats this into a list -- useful when there's lots of print out

-- verboseCheck is just like quickCheck, but shows you the tests generated

-- writing generators
-- using return, do

-- natural numbers
nats :: Gen Integer
nats =  fmap abs arbitrary 
{-  do
     n <- arbitrary
     return $ abs n
-}
evenInts :: Gen Integer
evenInts = fmap (*2) arbitrary
{-
evenInts = do 
         n <- arbitrary
         return $ 2 * n
-}

oddInts :: Gen Integer
oddInts = fmap (+1) evenInts
{-
         do 
         i <- evenInts
         return $ i + 1
-}

-- Building Generators
-- listOf, vectorOf
-- choose (,)
-- return
-- oneof
-- elements
-- frequency
---------------------------------------

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show,Eq)

instance Arbitrary Suit where
   arbitrary = rSuit

rSuit :: Gen Suit
rSuit = elements [Spades,Hearts, Diamonds,Clubs]

data Rank = Numeric Integer | Jack | Queen | King | Ace
                             deriving (Show,Eq,Ord)
                                      
rRoyal = elements [Jack , Queen , King , Ace]

rNumeric = fmap Numeric $ choose(2,10)

rRank = oneof [rRoyal,rNumeric]

instance Arbitrary Rank where
  arbitrary = frequency [(4,rRoyal),(9,rNumeric)]

prop_Rank'' r = classify (r < Jack) "Numeric" $ prop_Rank r
prop_Rank' r = collect r $ prop_Rank r
prop_Rank (Numeric n) = n >= 2 && n <= 10
prop_Rank _           = True

data Card = Card Rank Suit   
  deriving (Show,Eq)

instance Arbitrary Card where
   arbitrary = liftM2 Card arbitrary arbitrary
{-     
     do
             r <- arbitrary
             s <- arbitrary
             return $ Card r s
-}
data Hand = Empty | Add Card Hand 
   deriving (Show,Eq)

fromHand Empty = []
fromHand (Add c h) = c : fromHand h

toHand = foldr Add Empty

instance Arbitrary Hand where
  arbitrary = fmap (toHand . nub) $ listOf arbitrary
  
prop_Hand h = cs == nub cs where cs = fromHand h

--------------------------------------------------------------


-- Testing with Data Invariants
-- properties of insert

-- How to use a different generator for lists?
-- (i) use QuickCheck function forAll [not covered here: see qc tutorial]
-- (ii) Make a new type from the old with its own generator

{- -- Insertion sort from L02A

isort [] = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
insert x (y:ys)          = y:insert x ys

-}

prop_insert x xs  = 
    classify (length xs < 2) "Trivial" $ 
    collect (length xs) $
    sorted xs ==> sorted $ insert x xs 
  where types = x :: Integer

sorted xs = xs == sort xs -- inefficient! 
-- Exercise: define an O(n) version
-- Harder Exercise: define it without recursion (hint: zipWith)

newtype OrderedI = OrderedI [Integer]
  deriving (Eq,Show)

instance Arbitrary OrderedI where
  arbitrary = do
            xs <- arbitrary
            return $ OrderedI $ sort xs
            -- inefficient again. See slides for an O(n) version
            -- Exercise: redefine using liftM instead of do...

prop_insert2 x (OrderedI xs)  = sorted xs ==>
                                collect (length xs) $ 
                                classify (length xs < 2) "Trivial" $ 
                                sorted $ insert x xs 
                   where types = x :: Integer

-- Note that QuickCheck has a predefined generator for ordered lists
-- orderedList :: (Arbitrary a, Ord a) => Gen [a]
