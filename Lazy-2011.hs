-- examples from lecture on lazy evaluation
-- dave@chalmers.se 2011-11-28

-- for timing
import Data.Time.Clock(getCurrentTime, diffUTCTime)

import Data.Char
import Network.HTTP.Base(urlEncode)
import Data.List(foldl')


primes :: [Integer] 
primes = sieve [2..] 
 
sieve (x:xs) = 
     x : sieve [ y | y <- xs, y `mod` x /= 0 ]
 
-----------------------------------------------
primes' :: [Integer]
primes' = 2 : [ y | y <- [3,5..], 
                and [ y `mod` x /= 0| x <- primesToRoot y ] ]
 where
  primesToRoot x = let r =  floor . sqrt . fromInteger $ x 
                   in takeWhile (<= r) primes'

---------------------------------------

headFile f = do 
  c <- readFile f
  let c' = unlines . take 5 . lines $ c 
  putStrLn c'


-- lazy IO example
encodeLines = interact $ 
   unlines . map urlEncode . lines 

---------------------------------------------
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' = foldl' (+) 0

million = 1000000 :: Integer  

average :: [Integer] -> Integer
average xs = sum xs `div` 
             fromIntegral (length xs)

average' = uncurry div . foldl' f (0,0)
   where f (s,len) n = let s'   = s + n
                           len' = 1 + len
                       in s' `seq` len' `seq` (s',len')

----------------------------------------------


speedTest e1 e2 = do
  t1 <- getCurrentTime
  print e1 -- forces the evaluation of e1 now
  t2 <- getCurrentTime
  print e2
  t3 <- getCurrentTime
  putStrLn ("first expression:  " ++ show (t2 `minus` t1))
  putStrLn ("second expression: " ++ show (t3 `minus` t2))     
       where minus = diffUTCTime 
