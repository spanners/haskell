module BlackJack where
import Cards
import Wrapper
import System.Random

-- |An empty hand.
empty :: Hand
empty = Empty

-- |Given a hand, calculate the value of the hand, 
-- according the rules of Black Jack.
value :: Hand -> Integer
value Empty     = 0
value (Add (Card Ace _) h) -- The value of an Ace depends on the value of 
			   -- the rest of the hand 
                = if rest < 11 then 11 + rest 
                  else 1 + rest
                    where 
                      rest = value h
value (Add c h) = valueCard c + value h

-- |Given a card, calculate it's value.
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- |Given a rank, calculate it's value.
valueRank :: Rank -> Integer
valueRank (Numeric r) = r
valueRank         Ace = 11
valueRank          _  = 10

-- |Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- |Determine the winner of the game according to the rules of Black Jack.
winner :: Hand -> Hand -> Player
winner g b | gameOver g = Bank
winner g b = if value g > value b then Guest
             else Bank

-- |Given two hands, <+ puts the first one on top of the second one.
(<+) :: Hand -> Hand -> Hand
Empty         <+ h     = h
(Add c h1)    <+ h2    = Add c (h1 <+ h2)

-- |A full deck of 52 cards
fullDeck :: Hand
fullDeck = fullHand Hearts    <+ (fullHand Spades <+ 
           (fullHand Diamonds <+ fullHand Clubs))

-- |Given a suit, return a 13 card hand for that suit.
fullHand :: Suit -> Hand
fullHand s = fullNums s <+ Add (Card Jack s)
                            (Add (Card King s)
                             (Add (Card Queen s)
                              (Add (Card Ace s) Empty)))

-- |Given a suit, return a hand of all the numeric cards for that suit.
fullNums :: Suit -> Hand
fullNums s = toHand $ map (\x -> Card (Numeric x) s) [2..10]

-- |Draw a card into the players hand from the deck, and return the new values
-- of (hand, deck).
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _       = error "draw: The deck is empty"
draw (Add c h1) h2 = (h1, Add c h2)

-- |Play a bank move according to the rules of Black Jack.
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty
  where
    -- A helper function for playBank which keeps on drawing from the deck
    -- (until the bank's hand value is > 16).
    playBank' deck bank = if value bank' <= 16 
                        then playBank' deck' bank'
			  else bank
                          where (deck', bank') = draw deck bank

-- |Given a hand and a number n, remove the nth card from the hand,
-- resulting in a tuple (card, hand_without_card).
removeCard :: Hand -> Integer -> (Card, Hand)
removeCard Empty 0 = error "removeCard: card not in hand"
removeCard hand  n = removeCard' Empty hand n
  where 
    -- A helper function for removeCard which builds the remaining_hand into remainder.a
    removeCard' remainder (Add c h)     0 = (c, remainder     <+ h)
    removeCard' remainder (Add c Empty) n = (c, remainder)
    removeCard' remainder (Add c h)     n = removeCard' 
                                            ((Add c Empty) <+ remainder) h (n-1)
                                 
-- |Given a PRNG and a hand, shuffle the hand.
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g h     = shuffle' g Empty h 
  where                  
    -- A helper function for shuffle which stores the shuffled result so far.
    shuffle' g result Empty = result
    shuffle' g result m     = shuffle' g' (Add c Empty <+ result) h 
      where
    	(n, g') = randomR (0, size m) g
    	(c, h)  = removeCard  m n

-- |Given a card, is the card in the hand?
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
