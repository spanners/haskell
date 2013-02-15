module RefactoredParser
 ( Parser -- exports the type name but not the constructors
  ,parse,
  success,failure,sat,pmap,item,char,digit,
  (+++),(<:>),(>*>),(>->),(<-<),
  oneOrMore,zeroOrMore,chain
 ) 
{----------------------
Week 4B part II

Aim: reusable Parser combinators including 
 a new type for the Parser, 
 but no export of the constructor
----------------------}
where

import Data.Char
------------------

-- take the old type (in ReadExpr) which was a type synonym which is just the same as the old type except now it has a constructor on the front
-- so it's the same but is hidden inside a constructor
newtype Parser a = P (String -> Maybe (a,String))

-- Our library is exporting the type Parser, but not the function P
-- the only thing we'll be able to use to parse, is this parse function
parse :: Parser a -> String -> Maybe(a,String)
-- run a parser on a given string
-- pulls out the function from inside
parse (P f) = f

-------------------
-- Basic Parsers, dependent on internal structure -- 
-- success and fail
failure :: Parser a 
-- always fails
failure    = P $ \_ -> 
             Nothing

success :: a -> Parser a 
-- succeeds without looking at the string
success a  = P $ \s -> 
             Just (a,s)

item :: Parser Char
-- Parse any single character
item  = P $ \s ->
        case s of 
             (c:s') -> Just(c,s')
             _      -> Nothing

-- (+++)  parse either using p or else using q
infixr 5 +++
(+++) :: Parser a -> Parser a -> Parser a
p +++ q  = P $ \s ->
           case (parse p s) of
                Just (a,s') -> Just (a,s')
                Nothing     -> parse q s

-- (p >*> f) parse using p to produce a. 
-- Then parse using f a 

infixl 1 >*>

(>*>) :: Parser a -> (a -> Parser b) -> Parser b
p >*> f  = P $ \s -> 
           case parse p s of 
                Just(a,s') -> parse (f a) s'
                Nothing    -> Nothing

-- example: parse any lowercase letter 
-- followed by its uppercase equivalent aA or bB etc.

lowerUpper = item >*> \c -> char (toUpper c) 
-----------------------------------------------
-- Parsers below do not depend on the internal 
-- representation of Parser

-- sat p parse a single character satisfying property p
sat :: (Char -> Bool) -> Parser Char
sat p  = item >*> \c -> if p c then success c else failure 

char c = sat (==c)
digit  = sat isDigit

-- pmap modifies the result of a parser with function f
-- e.g. parse (digitToInt `pmap` digit) "123"
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = p >*> \a -> success (f a)

-- throw away what p gave, and parse with q
-- e.g.  parse (char '{' >-> digit) "{123}"
(>->) :: Parser a -> Parser b -> Parser b
p >-> q = p >*> \_ -> q

-- parses with p, and then just parses with q to make sure it's there and throws it away
-- e.g.  parse (char '{' >-> digit <-< char '}') "{1}xxx"
(<-<) :: Parser b -> Parser a -> Parser b
p <-< q = p >*> \a -> q >-> success a

(<:>):: Parser a -> Parser [a] -> Parser [a]
p <:> q = p >*> \a -> pmap (a:) q

oneOrMore, zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p +++ success []
oneOrMore p  = p <:> zeroOrMore p

chain :: Parser a -> Parser b -> Parser [a]
-- parse a list of as which are separated by bs
chain p q = p <:> zeroOrMore (q >-> p)

-- example: comma separated digits "1,2,3"
diglist = chain digit (char ',') 
