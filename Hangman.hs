-- Hangman game, Lecture 3A
-- 2012-11-12
-- Dave Sands
import Data.List(union,(\\))
import System.Random(randomRIO)

wordFile = "/usr/share/dict/words" -- mac OS X
guessLimit = 10

main :: IO()
main = do 
  w <- randomWord
  gameLoop w ""

randomWord :: IO String
randomWord = do 
  wlist <- readFile wordFile
  let ws = words wlist
  n <- randomRIO (0, length ws - 1)
  return (ws !! n)

gameLoop :: String -> String -> IO()
gameLoop w g | win       = showWin
             | lose      = showLose
             | otherwise = 
   do displayStatus
      guesses <- getLine
      gameLoop w (g `union` take lives guesses)  
  where
    win           = all (`elem` g) w
    lose          = lives <= 0
    lives         = guessLimit - length (g \\ w)
    showLose      = putStrLn $ "You die! The word was " ++ w
    showWin       = putStrLn $ "Win! You guessed " ++ w 
                             ++ " with " ++ show lives 
                             ++ " lives remaining"
    displayStatus = do putStrLn [if c `elem` g then c else '_' | c <- w] 
                       putStrLn $ "Type your guess (" ++ show lives ++ " remaining)"
          