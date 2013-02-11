module ReadExpr where
-- 2012-12-22 Week 4B Part 1
-- A brute-force quick and ugly 
-- parser for expressions.

-- Will be refactored into a parser 
-- library (RefactoredParser.hs)
-- and then used to parse expressions
-- (RefactoredReadExpr.hs)

import Data.Char(isSpace)
import Data.Maybe
import Test.QuickCheck

------------------------------------
data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
 deriving (Eq
          ,Show
          )
-------------------------------------------------------------
type Parser a = String -> Maybe (a,String)

num :: Parser Expr
num s = case reads s of
   (i,s'):_ -> Just (Num i,s')
   _        -> Nothing

-- | Chain together expression
chain :: 
  Eq a => 
  ([a] -> Maybe (t, [a])) 
  -> a -> (t -> t -> t) -> [a] -> Maybe (t, [a])
chain 
  -- | Parser p
  p 
  -- | Separated by character c
  c 
  -- | Built using function f 
  f 
  -- | Input expression string s
  s 
  = 
  case p s of 
    Just (n,c':s') | c == c' -> case chain p c f s' of 
      Just(e,s'') -> Just (f n e,s'')
      Nothing -> Just (n,c:s')
    r               -> r
  
-- <expression> ::= <term> | <term> "+" <expression>
expr = chain term '+' Add

-- <term>       ::= <factor> | <factor> "*" <term>
term = chain factor '*' Mul

-- <factor>     ::= "(" <expression> ")" | <number>
factor ('(':s) = case expr s of
   Just(e,')':s') -> Just (e, s')
   _              -> Nothing
factor s       = num s

-- top level parser
-- remove spaces and expect no junk!
readExpr :: String -> Maybe Expr              
readExpr s = case expr (filter (not . isSpace) s) of
  Just (e, "") -> Just e
  _            -> Nothing


------------------------------------------------
-- showing and testing
-- instance Show Expr where 
--    show = showExpr

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(s,rBin s)]  where 
   range = 4 -- integer range
   rNum = elements $ map Num [-range..range]
   rBin s = do 
        let s' = (s `div` 2)
        op <- elements [Mul,Add]
        e1 <- rExpr s' 
        e2 <- rExpr s'
        return $ op e1 e2   

instance Arbitrary Expr where
  arbitrary = sized rExpr

prop_readExpr e = let s       = showExpr e 
                      Just e' = readExpr s
                  in showExpr e' == s
