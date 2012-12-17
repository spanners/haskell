import Test.QuickCheck
import Control.Monad

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

game :: IO ()
-- use the binary chop algorithm
-- start off by picking half of possible range
-- e.g. if possible range is [1..100], pick 50
-- if higher, go for half of [51..100]
-- if lower, go for [1..49]
-- etc
game = 
  do 
    putStrLn "Think of a number between 1 and 100!"
    putStrLn "Is it 50?"
    hint <- getLine 
    if hint == "higher"
       then putStrLn "So it's higher"
      else if hint == "lower"
           then putStrLn "So it's lower"
           else if hint == "yes"
                then putStrLn "Great, I won!"
                else putStrLn "poop!"
    return ()

midList xs = xs !! ((length xs) `div` 2)