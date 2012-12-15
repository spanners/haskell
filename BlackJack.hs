module BlackJack where
import Test.QuickCheck
import Cards
import Wrapper

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

-- given a hand, determine the number of aces
numberOfAces :: Hand -> Integer
numberOfAces Empty     = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add c h) = numberOfAces h

prop_NumberOfAces h = numberOfAces h == (acesIn $ fromHand h)
  where acesIn []                = 0
        acesIn ((Card Ace _):cs) = 1 + acesIn cs
        acesIn (_:cs)            = acesIn cs
    

-- given a card, calculate it's value
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

prop_ValueCardSane (Card Ace s)         = valueCard (Card Ace s)         == 11
prop_ValueCardSane (Card (Numeric r) s) = valueCard (Card (Numeric r) s) == unpack (Numeric r) 
  where unpack (Numeric r) = r
prop_ValueCardSane c                    = valueCard c                    == 10

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

{- doesn't work yet...
prop_Winner (Add (Card Ace Spades) (Add (Card Ace Clubs) Empty)) b = winner (Add (Card Ace Spaces) (Add (Card Ace Clubs) Empty)) b
-}

-- given two hands, <+ puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
h             <+ Empty = h
Empty         <+ h     = h
(Add c h1)    <+ h2    = (Add c (h1 <+ h2))


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3 ) == (p1 <+ p2 ) <+ p3

prop_onTopOf_append h1 h2 = fromHand h1 ++ fromHand h2 == fromHand (h1 <+ h2)

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

fullDeck :: Hand
fullDeck = fullHand Hearts <+ (fullHand Spades <+ (fullHand Diamonds <+ fullHand Clubs))

fullHand :: Suit -> Hand
fullHand s = fullNums s <+ (Add (Card Jack s)
                            (Add (Card King s)
                             (Add (Card Queen s)
                              (Add (Card Ace s) Empty))))

fullNums :: Suit -> Hand
fullNums s = toHand $ map (\x -> (Card (Numeric x) s)) [2..10]

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _   = error "draw: The deck is empty"
draw (Add c h1) h2 = (h1, (Add c h2))

playBank :: Hand -> Hand
