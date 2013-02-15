module RefactoredReadExpr where
-- Lecture 4B part 2 2012-11-21
-- Use the refactored parser
import RefactoredParser
-- to build a parser for Expr
import Data.Char(isSpace)
import Data.Maybe
import Test.QuickCheck

data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
 deriving (Eq
          ,Show
          )
-- instance Show Expr where 
--    show = showExpr

-------------------------------------------------------------
u = undefined
-- parse an integer
-- with help from Read class
integer :: Parser Integer -- parse a natural number
integer = nat +++ pmap negate (char '-' >-> nat)
 where nat = oneOrMore digit >*> success . read  

num :: Parser Expr
num  = pmap Num integer

expr = foldr1 Add `pmap` chain term (char '+') 
   -- <expr> ::= <term> | <term> "+" <expr>
term = foldr1 Mul `pmap` chain factor (char '*')
-- <term> ::= <factor> | <factor> "*" <term>
factor = char '(' >-> expr <-< char ')'
     +++ num

  -- factor ::= "(" <expr> ")" | <number>

  
-- top level parser
-- remove spaces and expect no junk!
readExpr :: String -> Maybe Expr              
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing
----------------------------------------------
-- show, eval and quickCheck

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

eval :: Expr -> Integer
eval (Num n)    = n
eval (Add e1 e2)    = eval e1 + eval e2
eval (Mul e1 e2)= eval e1 * eval e2


rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(s,rOp)]
  where
   s' = s `div` 2
   rNum = do 
       n <- arbitrary
       return $ Num n

   rOp = do 
      op <- elements [Add,Mul]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ op e1 e2

instance Arbitrary Expr where
  arbitrary = sized rExpr

prop_readExpr e = let s = showExpr e
                      Just e' = readExpr s 
                  in showExpr e' == s