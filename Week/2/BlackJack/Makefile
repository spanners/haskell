all:
	ghc -o blackjack --make Main
	./blackjack

doc:
	haddock -h Main
	defaultbrowser index.html &

test:
	ghc -fhpc -o blackjack --make Main
	./blackjack
	hpc markup blackjack.tix --exclude=QC
	defaultbrowser hpc_index.html &

clean:
	rm -rf *.sty *.log *.tex *.pdf *.aux *.dvi *.out *.toc \
		*.o *.hi *.tix *.html blackjack .hpc *.png *.js \
		*.gif *.css dist/ ${HOME}/bin/BlackJack

package:
	cabal install --prefix=${HOME} --user
	cabal haddock --executable
