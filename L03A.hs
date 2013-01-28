module L03A where
import Data.List
import Data.Char
import Control.Monad
import System.FilePath.Posix


-- **** 
-- Small examples of IO
-- t: writeFile
-- :: FilePath -> String -> IO ()
-- t: readFile
-- :: FilePath -> IO String

-- * Building instructions vs running instructions
-- _Instructions_ themselves, and the _result_ of running the instructions are *not* the same
-- You should think of IO as a special, abstract type representing instructions

-- Functions that produce something of type IO _are_ pure functions! Whatever arguments you give them, they will always give the same instructions.

-- Who gets to run instructions? Is there a function called "runInstructions"? No. The only way they can be run is if they are the top-level value.


-- instructions with results:

-- building complex instructions from simple ones

copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFile toFile = 
  -- here you are getting a string from an IO String, but you're doing it INSIDE a `do`.
  -- So the only thing you can return is an IO ().
  -- readFile returns IO String,
  -- writeFile 2nd argument has to be a string.
   do c <- readFile fromFile 
      -- the type of the last line will be the type of the result
      -- here it is IO ()
      writeFile toFile c

-- var <- expr 
-- var :: a  expr :: IO 
-- this returns something of type IO a

-- find longest Word in "/usr/share/dict/words"
longest :: IO String
longest = do 
        wlist <- readFile "/usr/share/dict/words"
        -- we should keep the pure code in the definition of `long` separate from the do block
        -- it's much easier to test pure functional code which doesn't deal with IO
        return (long wlist)

  where long :: String -> String
        long  =  snd 
                 . maximum 
                 -- pairing two ordered things: length with the word itself
                 -- we get another ordered thing -- an ordered pair
                 -- with which we can find the maximum
                 . map (\w -> (length w, w))  
                 . words
-- dotwice

dotwice :: IO a -> IO (a,a)
dotwice i = do 
        a1 <- i
        a2 <- i
        return (a1,a2)

-- dont
dont :: IO a -> IO ()
-- this emphasises the difference between /having/ the instruction, and /running/ the instruction
-- instructions only get run when they meet the operating systems
dont i = return ()

test :: IO Int
test = do 
     return 42
     return 0

-- Exercises: Define the following functions: 
copyAll :: [FilePath] -> FilePath -> IO ()
copyAll fromFiles toDir = 
    sequence_ [copyFile f (combine toDir f) | f <- fromFiles]

concatAll :: [FilePath] -> FilePath -> IO ()
concatAll fromFiles toFile =
  (sequence . map readFile) fromFiles >>= writeFile toFile . unlines

forLoop :: (a -> IO ()) -> [a] -> IO ()
forLoop f = (sequence_ . map f) 
-- e.g. forLoop [1..10] print 
-- will print numbers from 1 to 10.
