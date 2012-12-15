import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)
----------------------------------------------------
-- Part 1
-- The computation takes k + 1 steps

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product $ replicate (fromInteger k) n

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | isEven k = power2 (n * n) (k `div` 2) 
  where
    isEven k = k `mod` 2 == 0
power2 n k = n * power2 n (k - 1)

-- Part 4
-- A.
-- n  k
-- 0  0        : Test edge cases
-- 0  1        : Test edge cases
-- 1  0        : Test edge cases
-- -1 0        : Test edge cases
-- 0  -1       : Test edge cases
-- 1  -1       : Test edge cases
-- -1 1        : Test edge cases
-- 99999 99999 : Test large numbers

-- B.
prop_powers n k =
  one == two && one == three && two == three 
   where one   = power  n k'
         two   = power1 n k'
         three = power2 n k'
         k' = abs k

--C.
-- Note: this does not run

testA = and $ prop_powers `zipWith` ns $ ks where
  ns = [0,0,1,-1,0,1,-1,99999]
  ks = [0,1,0,0,-1,-1,1,99999]


--D. Relies on C.