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

prop_value h1 h2 = if h1 == h2 then value h1 == value h2 else True

prop_gameOver h = if value h > 21 then gameOver h else not (gameOver h)

prop_empty = empty == Empty 

prop_winner_guestWins g b = winner g b == Bank || winner g b == Guest

prop_fullDeck = fullDeck == deck

prop_fullDeckLessStrict = isPermutation (fromHand fullDeck) (fromHand deck)

prop_draw h1 = h1 /= Empty ==>
               if size h1 == 1
                  then Empty == fst (draw h1 Empty)
               else Empty /= fst (draw h1 Empty) 

prop_playBank d = d /= Empty ==>
                  value (playBank d) <= 16

deck = Add (Card (Numeric 2) Hearts) 
	(Add (Card (Numeric 3) Hearts) 
	(Add (Card (Numeric 4) Hearts) 
	(Add (Card (Numeric 5) Hearts) 
	(Add (Card (Numeric 6) Hearts) 
	(Add (Card (Numeric 7) Hearts) 
	(Add (Card (Numeric 8) Hearts) 
	(Add (Card (Numeric 9) Hearts) 
	(Add (Card (Numeric 10) Hearts) 
	(Add (Card Jack Hearts) 
	(Add (Card King Hearts) 
	(Add (Card Queen Hearts) 
	(Add (Card Ace Hearts) 
	(Add (Card (Numeric 2) Spades) 
	(Add (Card (Numeric 3) Spades) 
	(Add (Card (Numeric 4) Spades) 
	(Add (Card (Numeric 5) Spades) 
	(Add (Card (Numeric 6) Spades) 
	(Add (Card (Numeric 7) Spades) 
	(Add (Card (Numeric 8) Spades) 
	(Add (Card (Numeric 9) Spades) 
	(Add (Card (Numeric 10) Spades) 
	(Add (Card Jack Spades) 
	(Add (Card King Spades) 
	(Add (Card Queen Spades) 
	(Add (Card Ace Spades) 
	(Add (Card (Numeric 2) Diamonds) 
	(Add (Card (Numeric 3) Diamonds) 
	(Add (Card (Numeric 4) Diamonds) 
	(Add (Card (Numeric 5) Diamonds) 
	(Add (Card (Numeric 6) Diamonds) 
	(Add (Card (Numeric 7) Diamonds) 
	(Add (Card (Numeric 8) Diamonds) 
	(Add (Card (Numeric 9) Diamonds) 
	(Add (Card (Numeric 10) Diamonds) 
	(Add (Card Jack Diamonds) 
	(Add (Card King Diamonds) 
	(Add (Card Queen Diamonds) 
	(Add (Card Ace Diamonds) 
	(Add (Card (Numeric 2) Clubs) 
	(Add (Card (Numeric 3) Clubs) 
	(Add (Card (Numeric 4) Clubs) 
	(Add (Card (Numeric 5) Clubs) 
	(Add (Card (Numeric 6) Clubs) 
	(Add (Card (Numeric 7) Clubs) 
	(Add (Card (Numeric 8) Clubs) 
	(Add (Card (Numeric 9) Clubs) 
	(Add (Card (Numeric 10) Clubs) 
	(Add (Card Jack Clubs) 
	(Add (Card King Clubs) 
	(Add (Card Queen Clubs) 
	(Add (Card Ace Clubs) Empty)))))))))))))))))))))))))))))))))))))))))))))))))))

main :: IO Bool
runTests = $(quickCheckAll)
main = runTests

------------------------------------------------------------

--main :: IO ()
--main = runGame implementation