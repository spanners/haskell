all:
	ghc -o blackjack --make Main
	./blackjack

test:
	ghc -fhpc -o blackjack --make Main
	./blackjack
	hpc markup blackjack.tix --exclude=QC
	chromium-browser hpc_index.html

clean:
	rm  -r *.o *.hi *.tix *.html blackjack .hpc
