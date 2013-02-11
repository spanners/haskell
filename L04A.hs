-- Part 1: The expression game

import Test.QuickCheck
import Data.Maybe(fromJust)

data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
 deriving Eq
          
instance Show Expr where
  show = showExpr
          
type Table = [(String,Integer)]

-- make a new type just for generation of suitable tables
newtype Env = Env Table
  deriving Show
           
instance Arbitrary Env where 
      arbitrary = do 
          (l,m,n) <- arbitrary
          return $ Env [("x",l),("y",m),("z",n)]

eval :: Table -> Expr -> Integer
eval t e = eval' e where
  eval' (Num n)     = n
  eval' (Add e1 e2) = eval' e1 + eval' e2
  eval' (Mul e1 e2) = eval' e1 * eval' e2
  eval' (Var x)     = fromJust $ lookup x t 
                      {- look x t
  look k [] = error $ "No value for " ++ k 
  look k ((k',v):t) | k == k'   = v
                    | otherwise = look k t
  -}
  
ex1 = Mul (Add (Var "y") (Num 2)) (Var "x")
ex2 = Add (Var "x") (Mul (Num 2) (Var "y"))
ex3 = Num (-5) `Add` (Num 2 `Mul` Num 4)

vars :: Expr -> [String]
vars (Num n)     = []
vars (Var s)     = [s]
vars (Add e1 e2) = vars e1 ++ vars e2
vars (Mul e1 e2) = vars e1 ++ vars e2


showExpr :: Expr -> String
showExpr (Var x)     = x
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

range = 4
-- creates an Int out of the above Num due to a restriction in Haskell:
-- Types can be polymorphic, but once a Num is an Int for example, it can't then be an Integer. Once a type is coerced, it can only be used as that type from then on.
level = fromInteger range

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (1,rVar), (s,rBin s)]
  where
    rVar = elements $ map Var ["x","y","z"]
    rNum = elements $ map Num [-range..range]
    rBin s = do
      let s' = (s `div` 2)
      op <- elements [Add,Mul]
      e1 <- rExpr s'      
      e2 <- rExpr s'
      return $ op e1 e2
    
instance Arbitrary Expr where
  arbitrary = sized rExpr
  
getTable :: Env -> Table
getTable (Env t) = t

main :: IO()
main = do
  es <- sample' $ rExpr level
  ts <- sample' (arbitrary :: Gen Env)
  let e = head es
  let t = getTable $ (!!level) ts    
  putStrLn $ "What is the value of " ++ show e
  putStrLn $ "with this table: " ++ show t ++ " ?"
  ans <- getLine
  let v = show $ eval t e
  if (ans == v)
    then putStrLn "Correct!"
    else putStrLn $ "Wrong! Correct answer was: " ++ v
  main
  
  
-- data Maybe a = Nothing | Just a
-- Maybe models failure
-- It might return `a`, or it might return `Nothing`

-- fromJust :: Maybe a -> a