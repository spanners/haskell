import Test.QuickCheck
import Control.Monad
import System.Cmd

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
game = 
  do
    putStrLn $ "Think of a number between " ++ show lower ++ " and " ++ show higher ++ "!"
    gameLoop [lower..higher]
    where
      lower = 1
      higher = 100

-- bugs: 
--      * It doesn't autoguess when range is 1 number
--      * It doesn't catch the error when range is 1 number 

gameLoop :: [Integer] -> IO ()
gameLoop range =
  do
    putStr $ "Is it " ++ show (midList range) ++ "? "
    hint <- getLine
    if hint == "higher" then gameLoop [((midList range)+1)..(last range)]
      else if hint == "lower" then gameLoop [(head range)..((midList range)-1)]
           else if hint == "yes" then putStrLn "Great, I won!"
                else gameLoop range
    return ()
        
-- again, bug with this as it doesn't work with ranges length 1
midList xs = xs !! ((length xs) `div` 2)

