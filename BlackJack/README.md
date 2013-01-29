Blackjack
=========

This is the "Blackjack" game, the solution to Lab Assignment 2 (*spoilers
inside!*)

I put Blackjack in quotes as it does not adhere to true Blackjack rules, but
instead uses the rules described in the assignment.

http://www.cse.chalmers.se/edu/year/2012/course/TDA452/labs/2/

It has some nice additions which I think many Haskell users might find useful:

1. **make test** runs a series of QuickCheck property tests, and runs a code
   coverage analysis, launching an HTML summary using "defaultbrowser" (change
   to firefox or chrome or what have you)
2. **make package** packages the program up into a cabal, installing in $HOME.
3. **make doc** builds documentation using Haddock

You may want to use this build setup for your own projects to maintain a
clearer overview of your testing coverage and documentation.

(See the Makefile for the implementation of these features.)
