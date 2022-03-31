#!/bin/sh

cd $1
/Library/TeX/texbin/pdflatex -shell-escape -interaction=batchmode $2

rm -f *.aux
rm -f *.log
rm -f *.pdf
rm -f $2
