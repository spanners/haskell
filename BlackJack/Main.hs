module Main where
import BlackJack
import Cards
import Wrapper

implementation = Interface { iEmpty    = empty,
                             iFullDeck = fullDeck,
                             iValue    = value,
                             iGameOver = gameOver,
                             iWinner   = winner,
                             iDraw     = draw,
                             iPlayBank = playBank,
                             iShuffle  = shuffle
                           }

main :: IO ()
main = runGame implementation
