module L01B where
import Test.QuickCheck

-- Algebraic data types
-- David Sands 
-- 2012-11-01

-- The Suit
data Suit = Spades | Hearts | Diamonds | Clubs 
  deriving (Show,Eq)

data Colour = Red | Black
  deriving Show

colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour _      = Red

-- Rank: a datatype for the rank of a card
data Rank =  Numeric Int | Jack | Queen | King | Ace
  deriving (Show,Eq,Ord)

-- data Value = Two | Three | 
-- rankBeats rank1 rank2 checks if rank1 beats rank2

rankBeats :: Rank -> Rank -> Bool
-- when is one rank higher than another?
-- "longhand" definition in the lecture notes. 
-- Here we use "deriving Ord" to induce ordering operations
rankBeats r1 r2 = r1 > r2
-- rankBeats = (>)

prop_rankBeats r1 r2 = r1 == r2 |+| r1 `rankBeats` r2 
                                |+| r2 `rankBeats` r1

-- xor: example using pattern matching
infixr 2 |+|
True  |+| False = True
False |+| True  = True
_     |+| _     = False

-- Card: a data type containing a Rank and a Suit
data Card = Card Rank Suit
  deriving (Show,Eq)

rank (Card r _) = r
suit (Card _ s) = s

-- An alternative shorthand for the three lines above:
-- data Card = Card {rank::Rank, suit::Suit}
 

-- cardBeats card1 card2 checks if card1 beats card2
cardBeats :: Card -> Card -> Bool
-- w & wo pattern matching

-- cardBeats c1 c2 = suit c1 == suit c2 && rank c1 `rankBeats` rank c2
cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && r1 `rankBeats` r2

----------------------------------------------------------
-- Hand: datatype for a hand of cards

data Hand = Empty | Add Card Hand
  deriving Show
-- either Empty, or the result of Adding a card to another hand

exHand1 = Add (Card Ace Hearts) Empty
exHand2 = Add (Card Ace Spades) exHand1

-- handBeats: when does one hand beat a given card
-- handBeats :: Hand -> Card -> Bool
handBeats Empty      c = False
handBeats (Add c' h) c = c' `cardBeats` c || handBeats h c

-- chooseCard beat hand 
-- chooses a card from hand to play, 
-- when beat is the card to be beaten
   -- If the hand is only one card, play it
   -- If there is a choice,
   --    Select the best card from the rest of the hand
   --    Choose between it and the first card
   -- Principles
   --   1. Follow suit if possible 
   -- (like in "trick winning" games: whist, bridge, ...)
   --   2. Play lowest winning card if possible
   --   3. Play lowest losing card otherwise

-- version from the slides, refactored slightly

chooseCard :: Card -> Hand -> Card
chooseCard beat (Add c Empty) = c
chooseCard beat (Add c rest) 
  | bnb c  c' = c
  | bnb c' c  = c'
  | tnt c  c' = c
  | tnt c' c  = c'
  | otherwise = rankMin c c'
 where
    bnb c1 c2 = c1 `cardBeats` beat && not(c2 `cardBeats` beat) 
    tnt c1 c2 = suit c1 == suit beat && suit c2 /= suit beat
    
    c' = chooseCard beat rest
    rankMin c1 c2 
      | rankBeats (rank c1) (rank c2) = c2 
      | otherwise                     = c1

    
    -- Trumps-Non-Trumps: first card has same suit, second does not


-- property: relating chooseCard and handBeats
-- The hand can beat the card 
-- if and only if the chosen card can
prop_win c h = isNonEmpty h  ==> 
       h `handBeats` c == chooseCard c h `cardBeats` c 
  where isNonEmpty Empty = False
        isNonEmpty _     = True
-------------------------------------------------------------------------
-- The quickCheck "magic" we need to get it to generate arbitrary
-- elements of our new datatypes:
 
instance Arbitrary Suit where
  arbitrary = elements [Spades, Hearts, Diamonds, Clubs]

instance Arbitrary Rank where
  arbitrary =
    oneof $
      [ do return c
      | c <- [Jack,Queen,King,Ace]
      ] ++
      [ do n <- choose (2,10)
           return (Numeric n)
      ]

instance Arbitrary Card where
  arbitrary =
    do r <- arbitrary
       s <- arbitrary
       return (Card r s)

instance Arbitrary Hand where
  arbitrary =
    do cs <- arbitrary
       let hand []     = Empty
           hand (c:cs) = Add c (hand [ c' | c' <- cs, c' /= c ])
       return (hand cs)
  shrink Empty = []
  shrink (Add c h) = h : map (Add c) (shrink h)

-------------------------------------------------------------------------


