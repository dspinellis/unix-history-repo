# Copyright 1985, 1986 Massachusetts Institute of Technology
# Makefile for the Installation document.

TROFF= ditroff -Tpsc
PRINTER= lpr -n -Pln03-bldge40-4
FILES= 	in.1 \
	in.2 \
	in.3 \
	appendix

all: 

paper: $(FILES)
	eqn ${FILES} | tbl |${TROFF} -ms | ${PRINTER}

preview:$(FILES)
	neqn ${FILES} | tbl | nroff -ms | colcrt


clean:
	rm -f *~ \#* *.bak errs 
