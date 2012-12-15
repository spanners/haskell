module BlackJack where
import Cards
import Wrapper
import System.Random

-- returns an empty hand
empty :: Hand
empty = Empty

-- given a hand,
--  calculate the value of the hand according to 
--  the rules of BlackJack
value :: Hand -> Integer
value Empty     = 0
value (Add (Card Ace _) h) -- The value of an Ace depends on the value of the rest of the hand 
                = if rest < 11 then 11 + rest 
                  else 1 + rest
                    where 
                      rest = value h
value (Add c h) = valueCard c + value h

-- given a card, calculate it's value
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

prop_ValueCardSane  (Card Ace s) 
  = valueCard (Card Ace s)         == 11
prop_ValueCardSane (Card (Numeric r) s) 
  = valueCard (Card (Numeric r) s) == unpack (Numeric r) 
    where unpack (Numeric r) = r
prop_ValueCardSane c 
  = valueCard c                    == 10

-- given a rank, calculate it's value
valueRank :: Rank -> Integer
valueRank (Numeric r) = r
valueRank         Ace = 11
valueRank          _  = 10

prop_ValueRankSane r = v >= 2 && v <= 11 
  where v = valueRank r
        
-- given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- determine the winner of the game according to the rules of Black Jack
winner :: Hand -> Hand -> Player
winner g b | gameOver g = Bank
winner g b = if value g > value b then Guest
             else Bank

-- given two hands, <+ puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
h             <+ Empty = h
Empty         <+ h     = h
(Add c h1)    <+ h2    = (Add c (h1 <+ h2))


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = 
  p1 <+ (p2 <+ p3 ) == (p1 <+ p2 ) <+ p3

prop_onTopOf_append :: Hand -> Hand -> Bool
prop_onTopOf_append h1 h2 = 
  fromHand h1 ++ fromHand h2 == fromHand (h1 <+ h2)

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = 
  size h1 + size h2 == size (h1 <+ h2)

-- a full deck of 52 cards
fullDeck :: Hand
fullDeck = fullHand Hearts <+ (fullHand Spades <+ (fullHand Diamonds <+ fullHand Clubs))

-- given a suit, return a full hand for that suit
fullHand :: Suit -> Hand
fullHand s = fullNums s <+ (Add (Card Jack s)
                            (Add (Card King s)
                             (Add (Card Queen s)
                              (Add (Card Ace s) Empty))))

-- given a suit, return a hand of all the numeric cards for that suit
fullNums :: Suit -> Hand
fullNums s = toHand $ map (\x -> (Card (Numeric x) s)) [2..10]

-- draw a card into the players hand from the deck, and return the new values of (hand, deck)
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _       = error "draw: The deck is empty"
draw (Add c h1) h2 = (h1, (Add c h2))

-- play a bank move according to the rules of Black Jack
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- a helper function for playBank which keeps on drawing from the deck
-- until the bank's hand value is > 16
playBank' :: Hand -> Hand -> Hand
playBank' deck bank = if value bank' <= 16 
                      then playBank' deck' bank'
			else bank
                        where (deck', bank') = draw deck bank

-- given a hand and a number n, remove the nth card from the hand 
-- resulting in a tuple (card, hand_without_card)
removeCard :: Hand -> Integer -> (Card, Hand)
removeCard Empty 0 = error "removeCard: card not in hand"
removeCard hand  n = removeCard' Empty hand n


-- a helper function for removeCard which builds the remaining_hand into remainder
removeCard' :: Hand -> Hand -> Integer -> (Card, Hand)
removeCard' remainder (Add c h)     0 = (c, (remainder     <+ h))
removeCard' remainder (Add c Empty) n = (c, remainder)
removeCard' remainder (Add c h)     n = removeCard' 
                                            ((Add c Empty) <+ remainder) h (n-1)
                                 
-- given a PRNG and a hand, shuffle the hand
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g h     = shuffle' g Empty h
                  
-- a helper function for shuffle which stores the shuffled result so far
shuffle':: StdGen -> Hand -> Hand -> Hand
shuffle' g result Empty = result
shuffle' g result curr  = shuffle' g' ((Add c Empty) <+ result) h 
  where (n,g') = randomR    (0, (size curr)) g
        (c,h)  = removeCard curr n

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h =
  size h == size (shuffle g h)

-- given a card, is the card in the hand?
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
