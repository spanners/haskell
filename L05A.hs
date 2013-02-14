-- L05A Monads 
import Test.QuickCheck
import Parsing
import Data.Char

blist = do 
  char '{'
  ds <- chain digit (char ',')
  char '}'
  return $ map digitToInt ds

blist' = char '{' >> do 
  ds <- chain digit (char ',')
  char '}'
  return $ map digitToInt ds

blist'' = char '{' >> chain digit (char ',') >>= \ds -> 
            char '}' >>
              return (map digitToInt ds)



prop_blist = 
 parse blist "{1,2,3,4}xxx" == Just ([1,2,3,4], "xxx")

doTwice i = do
  a <- i
  b <- i
  return (a,b)

doTwice' i = i >>= \a -> 
            i >>= \b -> 
            return (a,b)

-----------------------------------------
{--- Example of another Monad: Maybe
-- from the Prelude:
 instance Monad Maybe where
    return         = Just

    Nothing  >>= f = Nothing
    (Just x) >>= f = f x

-----------------------------------------}

type CarReg = String ; type PNr = String  
type Name = String   ; type Address = String

carRegister :: [(CarReg,PNr)]
carRegister
 = [("FYN 433","850219-1234"),
    ("GYN 434","850219-1234"),
    ("JBD 007","750408-0909")]

nameRegister :: [(PNr,Name)]
nameRegister 
 = [("750408-0909","Dave")
   ,("850219-1234","Bob") 
   ,("890929-C234","John")]             

addressRegister :: [((Name,PNr),Address)]
addressRegister = 
  [(("Dave","750408-0909"),"42 Streetgatan\n Askim")
  ,(("Bob","850219-1234") ,"1 Chalmers Av\n Gothenburg") ]


billingAddress :: CarReg -> Maybe (Name, Address)
-- given a registration number, 
-- returns the name and address of owner, if defined. 

billingAddress car = 
 case lookup car carRegister of
   Nothing -> Nothing
   Just pnr -> case lookup pnr nameRegister of
        Nothing -> Nothing
        Just name -> 
           case lookup (name,pnr) addressRegister of
                Nothing -> Nothing
                Just addr -> Just (name,addr)

-- Monadic style:
billingAddress' car = do 
  pnr  <- lookup car carRegister
  name <- lookup pnr nameRegister
  addr <- lookup (name,pnr) addressRegister
  return (name,addr)

-- one step of de-sugaring:
billingAddress'' car = 
  lookup car carRegister >>= \pnr -> 
 do 
  name <- lookup pnr nameRegister
  addr <- lookup (name,pnr) addressRegister
  return (name,addr)

-- all de-sugaring
billingAddress''' car = lookup car carRegister >>= \pnr ->
  lookup pnr nameRegister >>= \name ->
  lookup (name,pnr) addressRegister >>= \addr ->
  return (name,addr)

test' = do 
     return 32
     x <- Nothing
     return 42

test'' =  
     return 32 >>= \_ -> 
     Nothing   >>= \x -> return 42

test''' = 
  billingAddress''' "GYN 434" == Just ("Bob","1 Chalmers Av\n Gothenburg")

-- The fail function gives an error by default
test = do putStrLn "hello"
          "42" <- getLine
          putStrLn "World"
