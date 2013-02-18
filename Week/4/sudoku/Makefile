test:
	ghc -fhpc -o sudoku --make Main
	./sudoku
	hpc markup sudoku.tix --exclude=QC
	defaultbrowser hpc_index.html &

clean:
	rm -rf *.sty *.log *.tex *.pdf *.aux *.dvi *.out *.toc \
		*.o *.hi *.tix *.html sudoku .hpc *.png *.js \
		*.gif *.css dist/ ${HOME}/bin/sudoku
