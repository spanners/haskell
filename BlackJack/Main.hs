{-# LANGUAGE TemplateHaskell #-} -- for running quickCheckAll

module Main where
import BlackJack
import Cards
import Wrapper
import System.Random
import Test.QuickCheck
import Test.QuickCheck.All

implementation = Interface { iEmpty    = empty,
                             iFullDeck = fullDeck,
                             iValue    = value,
                             iGameOver = gameOver,
                             iWinner   = winner,
                             iDraw     = draw,
                             iPlayBank = playBank,
                             iShuffle  = shuffle
                           }


-- Testing

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h =
  size h == size (shuffle g h)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = 
  p1 <+ (p2 <+ p3 ) == (p1 <+ p2 ) <+ p3

prop_onTopOf_append :: Hand -> Hand -> Bool
prop_onTopOf_append h1 h2 = 
  fromHand h1 ++ fromHand h2 == fromHand (h1 <+ h2)

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = 
  size h1 + size h2 == size (h1 <+ h2)

prop_ValueRankSane r = v >= 2 && v <= 11 
  where v = valueRank r


prop_ValueCardSane  (Card Ace s) 
  = valueCard (Card Ace s)         == 11
prop_ValueCardSane (Card (Numeric r) s) 
  = valueCard (Card (Numeric r) s) == unpack (Numeric r) 
    where unpack (Numeric r) = r
prop_ValueCardSane c 
  = valueCard c                    == 10


main :: IO Bool
runTests = $(quickCheckAll)
main = runTests

------------------------------------------------------------

--main :: IO ()
--main = runGame implementation
