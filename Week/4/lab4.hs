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
        
<<<<<<< HEAD
midList [] = error "midList: list length 0"
midList xs = xs !! ((length xs) `div` 2)

listOf' :: Integer -> Gen a -> Gen [a]
listOf' n g = sequence' [ g | _ <- [1..n] ]

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(1,rVar),(s,rOp)]
  where
   rVar = elements $ map Var ["x","y","z"]

   rNum = do 
       n <- arbitrary
       return $ Num n

   rOp = do 
      op <- elements [Add,Mul]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ op e1 e2

   s' = s `div` 2

instance Arbitrary Expr where
  arbitrary = sized rExpr

showExpr (Var x) = x
showExpr (Num n) = show n
showExpr (Add e e') = 
  showExpr e ++ " + " ++ showExpr e'
showExpr (Mul e e') = 
  showFactor e ++ " * " ++ showFactor e'

showFactor (Add e e') = 
  "(" ++ showExpr (Add e e') ++ ")"
showFactor e          = showExpr e

instance Show Expr
   where show = showExpr
         
data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
 deriving Eq
=======
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
>>>>>>> 195f2ba578e05cb4cb7bde7a5271fcb07eab7e16
