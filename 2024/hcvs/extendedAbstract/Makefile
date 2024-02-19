.PHONY: clean all dist

all:
	latexmk -pdf main.tex

clean:
	latexmk -C

dist: clean
	zip eptcsstyle.zip generic.bib eptcs.cls *.bst main.tex
