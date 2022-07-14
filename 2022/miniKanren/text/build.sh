#!/bin/sh

pdflatex main.tex &&
pdflatex main.tex &&
bibtex main.aux &&
pdflatex main.tex &&
evince main.*pdf &
