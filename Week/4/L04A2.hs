module L04A2 where
-- Lecture week 4A part 2
-- Symbolic expressions
-- dave@chalmers.se 
-- 2012-11-19
import Test.QuickCheck
import Data.Maybe(fromJust)
------------------------------------------------------
data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
 deriving Eq

ex1 = Mul (Add (Var "y") (Num 2)) (Var "x") 
ex2 = Add (Var "x") (Mul (Num 2) (Var "y"))
ex3 = Num (-5) `Add` (Num 2 `Mul` Num 4)

vars :: Expr -> [String]
vars (Num n)     = []
vars (Var s)     = [s]
vars (Add e1 e2) = vars e1 ++ vars e2
vars (Mul e1 e2) = vars e1 ++ vars e2

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
  look k [] = error $ " No value for " ++ k
  look k ((k',v):t) | k == k'   = v
                    | otherwise = look k t
-}

-- data Maybe a = Nothing | Just a
-- Maybe models failure -- in the case of lookup x t, there might be an x in the table, there might not be
  
-- See the example of take 10 [1..10^20]
-- Here, take asks for a result of the first 10 items
-- But it can't get any items until the list is built
-- So Haskell builds a bit of the list and then take returns a bit of the result
-- Then a bit more of the list is built etc.
-- It's fucking CHAOS!
-- There is no sense of control flow. There is no obvious order in which computation takes place in a Haskell program. Haskell computations are LAZY.

showExpr (Var x) = x
showExpr (Mul (Num n) (Var x)) = show n ++ x
showExpr (Mul (Var x) (Num n)) = show n ++ x
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

------------------------------------------------------
-- generators for expressions

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
-----------------------------------------------------------------------

derive :: String -> Expr -> Expr
-- derive x e 
-- the derivative of e with respect to x (d/dx) 
 
derive x (Add e1 e2) = add (derive x e1) (derive x e2)
derive x (Mul e1 e2) = add (mul (derive x e1) e2 ) 
                           (mul e1 (derive x e2))
derive x (Var y) | x == y = Num 1
derive _ _                = Num 0

add (Num n) (Num m) = Num (n+m)
add (Num 0) e       = e
add e       (Num 0) = e
add e1      e2      = Add e1 e2

mul (Num n) (Num m) = Num (n*m)
mul (Num 0) e       = Num 0
mul e       (Num 0) = Num 0
mul (Num 1) e       = e
mul e       (Num 1) = e
mul e1      e2      = Mul e1 e2


-- ***** simplify

-- * end
