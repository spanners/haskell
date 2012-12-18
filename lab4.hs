import Test.QuickCheck
import Control.Monad
import System.Cmd
import System.Random
import System.IO.Unsafe  -- be careful! 

look :: Eq a => a -> [(a,b)] -> Maybe b
look x []           = Nothing
look x ((x',y):xys)
  | x == x'         = Just y
  | otherwise       = look x xys
                      
                      
prop_LookNothing x t = 
  if look x t == Nothing
     then not (x `elem` (map fst t))
  else True
       
prop_LookJust x y t =
  if look x t == Just y
     then (x,y) `elem` t
  else True
       
prop_Look x y t = prop_LookNothing x t || prop_LookJust x y t

sequence' :: Monad m => [m a] -> m [a]
sequence' []     = return []
sequence' (m:ms) = do val <- m
                      vals <- sequence' ms
                      return (val:vals)
-- can also be written
-- sequence' = foldr (liftM2 (:)) (return [])   

files = ["hello.txt","world.txt"]

onlyIf :: Monad m => Bool -> m () -> m ()
onlyIf True  m = do m 
onlyIf False m = return ()

-- Q3(*).

game :: IO ()
game = 
  do
    putStrLn $ "Think of a number between " ++ show lower ++ " and " ++ show higher ++ "!"
    gameLoop [lower..higher]
    where
      lower = 1
      higher = 100

gameLoop :: [Integer] -> IO ()
gameLoop range =
  do
    putStr $ "Is it " ++ show guess ++ "? "
    hint <- getLine
    if hint == "higher" then gameLoop [(guess+1)..(last range)]
      else if hint == "lower" then gameLoop [(head range)..(guess-1)]
           else if hint == "yes" then putStrLn "Great, I won!"
                else gameLoop range
    where
      guess = midList range
        
midList [] = error "midList: length is 0"
midList xs = xs !! ((length xs) `div` 2)

-- Q5(*).

-- A.
listOf' :: Integer -> Gen a -> Gen [a]
listOf' n g = sequence' [g | _ <- [1..n]]

-- B.

listOfPairs :: Gen [([Int], [Int])]
listOfPairs = 
  do 
    n <- arbitrary
    (x,y) <- arbitrary
    listOf' n (return ([x::Int], [y::Int]))

-- C.
--prop_unzipInverseOfZip :: Eq a => Eq b => [a] -> [b] -> Bool
--prop_unzipInverseOfZip a b = unzip (zip a b) == (a, b)
