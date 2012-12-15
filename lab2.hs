-- Black Jack
-- Two players:
--   Guest -- plays first, can draw as many cards as they wish as long as the total <= 21. 
--             When they want to stop or when they're bust (score > 21), the Bank plays
--   Bank  -- plays second, draws cards until it's score is >= 16, then stops.

-- The value of a hand (the score) is the sum of the values of the cards. The values are as follows:

-- Numeric cards have their numeric value, so a nine is worth 9 points.

-- Jacks, queens and kings are worth 10 points

-- Aces are worth either 1 point or 11 points. When calculating the value for a hand, all aces have the same value. Either they are all worth 11 points, or they are all worth 1 point. Initially the value of 11 is used for the aces, but if that leads to a score > 21, then the value 1 is used instead

-- The winner is the player with the highest score that does not exceed 21. If the players end up with the same score, then the bank wins. The bank also wins if both players go bust.