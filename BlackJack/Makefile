all:
	ghc -o blackjack --make Main
	./blackjack

docs:
	haddock -h Main
	chromium-browser index.html &

test:
	ghc -fhpc -o blackjack --make Main
	./blackjack
	hpc markup blackjack.tix --exclude=QC
	chromium-browser hpc_index.html &

clean:
	rm -rf *.sty *.log *.tex *.pdf *.aux *.dvi *.out *.toc *.o *.hi *.tix *.html blackjack .hpc *.png *.js *.gif *.css
